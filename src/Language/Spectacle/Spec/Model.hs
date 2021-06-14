module Language.Spectacle.Spec.Model
  ( stepModel,
    throwRuntimeException,
  )
where

import Control.Monad.Except
  ( MonadError (catchError, throwError),
    forM,
    forM_,
    unless,
    void,
    when,
  )
import Control.Monad.Reader (MonadReader (local))
import Data.Bifunctor (Bifunctor (second))
import Data.Either (partitionEithers, rights)
import qualified Data.HashMap.Strict as HashMap
import Data.Maybe (isJust)
import Data.Sequence (pattern (:<|), pattern (:|>))
import Data.Set (Set)
import qualified Data.Set as Set
import Debug.Trace (trace)
import Lens.Micro (to, (%~), (&), (<>~), (^.))
import Lens.Micro.Mtl (use, view, (%=))

import Data.Type.Rec (Rec)
import Language.Spectacle.AST
  ( runAction,
    runInvariant,
    runTerminate,
  )
import Language.Spectacle.Exception (SpecException (ModelCheckerException, RuntimeException))
import Language.Spectacle.Exception.ModelCheckerException
  ( FormulaException,
    ImpassException (ImpassFailedTerminate, ImpassInfiniteStutter, ImpassNoTermination),
    ModelCheckerException
      ( FormulaException,
        ImpassException,
        TerminationException
      ),
    TerminationException (UnsatisfiedImplication),
  )
import Language.Spectacle.Exception.RuntimeException (RuntimeException)
import Language.Spectacle.RTS.Registers (RuntimeState (newValues))
import Language.Spectacle.Spec.Base
  ( Fairness (StronglyFair, Unfair, WeaklyFair),
    HasImpliedFormula (impliedFormula),
    Specifiable,
  )
import Language.Spectacle.Spec.Behavior (Behavior)
import Language.Spectacle.Spec.CheckResult
  ( CheckResult,
    flattenImplicationTree,
    isComplete,
    isSatisfied,
  )
import Language.Spectacle.Spec.Coverage (HasCoverageMap (coverageMap), emptyCoverageInfo, subsequentWorlds)
import Language.Spectacle.Spec.Implication (Implication (Implication), hasPartialCorrectness)
import Language.Spectacle.Spec.Model.Base
  ( Model,
    modelAction,
    modelFairness,
    modelFormula,
    modelJunctions,
    modelTerminate,
    modelTrace,
    worldHere,
  )
import Language.Spectacle.Spec.Prop
  ( PropCtx (PropCtx),
    PropState (PropState),
    checkInvariance,
    infoHere,
    infoThere,
    runProp,
  )

-- ---------------------------------------------------------------------------------------------------------------------

stepModel :: forall ctx. Specifiable ctx => Rec ctx -> Model ctx [Behavior ctx]
stepModel world = do
  info <- use (coverageMap . to (HashMap.lookup world))

  nextWorlds <- takeQuotient world
  case nextWorlds of
    Left _ -> do
      _ <- checkTermination False
      pure <$> view modelTrace
    Right worlds
      | null worlds -> do
        -- The accessibility relation yields an empty set, this is equivalent to the next-state being disabled.
        behavior <- view modelTrace
        fairness <- view modelFairness
        if fairness == Unfair
          then do
            -- In the case of an unfair, disabled action, the model's invariant must be completely satisfied for an
            -- infinite extension of stuttering steps and by the termination condition (if there is one).
            checkInfiniteStutter world
            hasTermination <- isJust <$> view modelTerminate
            when hasTermination (void (checkTermination False))
            return [behavior :|> world]
          else do
            -- For weak and strong fair processes, we need to show that the process satisfies the termination condition
            -- (if there was one provided), or that an indefinite stutter-steps is valid behavior.
            stutterResult <-
              checkStutterInvariance world >>= \case
                Left _ -> return False
                Right result -> return (result ^. isComplete)
            termResult <- (checkTermination False >> return True) `catchError` (const . return $ False)
            if stutterResult || termResult
              then return [behavior :|> world]
              else throwImpassException (ImpassInfiniteStutter world)
      | otherwise -> do
        -- The next-state yields a non-empty set of worlds, the action is enabled.
        fairness <- view modelFairness
        case fairness of
          Unfair -> do
            trace (show world ++ ", info: " ++ show info) (pure ())
            stepUnfair world worlds
          WeaklyFair -> weaklyContinueFrom world worlds
          StronglyFair -> stronglyContinueFrom world worlds

stepUnfair :: Specifiable ctx => Rec ctx -> [Rec ctx] -> Model ctx [Behavior ctx]
stepUnfair here theres = checkInfiniteStutter here >> weaklyContinueFrom here theres

weaklyContinueFrom :: Specifiable ctx => Rec ctx -> [Rec ctx] -> Model ctx [Behavior ctx]
weaklyContinueFrom here theres =
  concat <$> forM theres \there -> do
    checkStepInvariance True here there >>= \case
      Left exc -> throwFormulaException exc
      Right result
        | result ^. isComplete -> pure <$> view modelTrace
        | otherwise -> do
          let implications = flattenImplicationTree (result ^. impliedFormula)
          concat <$> forM implications \propertySet ->
            flip local (stepModel there) \context ->
              context
                & modelTrace %~ (here :<|)
                & impliedFormula <>~ propertySet

stronglyContinueFrom :: forall ctx. Specifiable ctx => Rec ctx -> [Rec ctx] -> Model ctx [Behavior ctx]
stronglyContinueFrom here theres = do
  behaviors <-
    second concat . partitionEithers <$> forM theres \there -> do
      checkStepInvariance True here there >>= \case
        Left exc -> return (Left exc)
        Right result
          | result ^. isComplete -> do
            behavior <- pure <$> view modelTrace
            return (Right behavior)
          | otherwise -> do
            let implications = flattenImplicationTree (result ^. impliedFormula)
            branches <- concat . rights <$> branchImplications there implications
            return (Right branches)
  if null (snd behaviors)
    then mapM throwFormulaException (fst behaviors)
    else return (snd behaviors)
  where
    branchImplications :: Rec ctx -> [Set Implication] -> Model ctx [Either SpecException [Behavior ctx]]
    branchImplications there implications =
      forM implications \propertySet ->
        let stepped = flip local (stepModel there) \context ->
              context
                & modelTrace %~ (here :<|)
                & impliedFormula <>~ propertySet
         in catchError (Right <$> stepped) (return . Left)

-- | Check if the model's invariant would be satisfied if an infinite sequence of stutter-steps extended model's
-- behavior.
--
-- @since 0.1.0.0
checkInfiniteStutter :: Specifiable ctx => Rec ctx -> Model ctx ()
checkInfiniteStutter world = trace "<<stutter>>" do
  checkStutterInvariance world >>= \case
    Left exc -> throwFormulaException exc
    Right result
      | result ^. isSatisfied -> return ()
      | otherwise -> throwImpassException (ImpassInfiniteStutter world)

checkStutterInvariance :: Specifiable ctx => Rec ctx -> Model ctx (Either FormulaException CheckResult)
checkStutterInvariance world = checkStepInvariance False world world

checkStepInvariance :: Specifiable ctx => Bool -> Rec ctx -> Rec ctx -> Model ctx (Either FormulaException CheckResult)
checkStepInvariance isEnabled here there = do
  propState <-
    PropState
      <$> use (coverageMap . to (HashMap.lookupDefault emptyCoverageInfo here))
      <*> use (coverageMap . to (HashMap.lookupDefault emptyCoverageInfo there))
  propCtx <-
    PropCtx
      <$> use coverageMap
      <*> pure here
      <*> pure there
      <*> view modelTrace
      <*> view modelJunctions
  terms <- view modelFormula >>= either throwRuntimeException pure . runInvariant isEnabled here there
  let (result, propState') = runProp propState propCtx (checkInvariance terms)
  coverageMap %= HashMap.insert here (propState' ^. infoHere)
  coverageMap %= HashMap.insert there (propState' ^. infoThere)
  return result

checkTermination :: Specifiable ctx => Bool -> Model ctx Bool
checkTermination isEnabled = do
  here <- view worldHere
  view modelTerminate >>= \case
    Nothing -> throwImpassException (ImpassNoTermination here)
    Just terminate
      | isEnabled -> do
        -- If the next-state relation is enabled and the termination condition is not satisfied so we don't emit
        -- exceptions here.
        implications <- view impliedFormula
        let canTerminate = runTerminate True here terminate
            passesPartial = partiallyCorrect (Set.toList implications)
        return (canTerminate && passesPartial)
      | otherwise -> do
        -- When the next-state relation is not enabled, the termination condition /must/ be satisfied, otherwise the
        -- model checker has reached an impass.
        implications <- view impliedFormula
        forM_ implications \(Implication name modality _) ->
          unless (hasPartialCorrectness modality) do
            throwTerminationException (UnsatisfiedImplication name modality here)
        if runTerminate True here terminate
          then return True
          else throwImpassException (ImpassFailedTerminate here)
  where
    partiallyCorrect :: [Implication] -> Bool
    partiallyCorrect [] = True
    partiallyCorrect (Implication _ modality _ : xs)
      | hasPartialCorrectness modality = partiallyCorrect xs
      | otherwise = False

-- | Runs the next-state relation for this model to yield a set of worlds accessible from the given world.
--
-- @since 0.1.0.0
takeQuotient :: Specifiable ctx => Rec ctx -> Model ctx (Either SpecException [Rec ctx])
takeQuotient world =
  view (modelAction . to (runAction world)) >>= \case
    Left exc -> return (Left (RuntimeException exc))
    Right xs -> do
      let worlds = Set.fromList (filterUnrelated xs)
      coverageMap %= flip HashMap.adjust world \info ->
        info & subsequentWorlds <>~ worlds
      return (Right (Set.toList worlds))
  where
    filterUnrelated :: [(RuntimeState ctx, Bool)] -> [Rec ctx]
    filterUnrelated = \case
      [] -> []
      (x, True) : xs -> newValues x : filterUnrelated xs
      (_, False) : xs -> filterUnrelated xs

throwModelCheckerException :: ModelCheckerException -> Model ctx a
throwModelCheckerException = throwError . ModelCheckerException
{-# INLINE throwModelCheckerException #-}

throwRuntimeException :: RuntimeException -> Model ctx a
throwRuntimeException = throwError . RuntimeException
{-# INLINE throwRuntimeException #-}

throwFormulaException :: FormulaException -> Model ctx a
throwFormulaException = throwModelCheckerException . FormulaException
{-# INLINE throwFormulaException #-}

throwImpassException :: ImpassException -> Model ctx a
throwImpassException = throwModelCheckerException . ImpassException
{-# INLINE throwImpassException #-}

throwTerminationException :: TerminationException -> Model ctx a
throwTerminationException = throwModelCheckerException . TerminationException
{-# INLINE throwTerminationException #-}
