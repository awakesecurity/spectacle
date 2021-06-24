module Language.Spectacle.Spec.Model
  ( stepModel,
    throwRuntimeException,
  )
where

import Control.Monad.Except
  ( MonadError (catchError, throwError),
    forM,
    unless,
  )
import Control.Monad.Reader (MonadReader (local), asks)
import Data.Either (lefts, rights)
import qualified Data.HashMap.Strict as HashMap
import Data.Sequence (pattern (:|>))
import qualified Data.Set as Set
import Lens.Micro (to, (&), (.~), (<>~), (^.))
import Lens.Micro.Mtl (use, view, (%=))

import Data.Type.Rec (Rec)
import Language.Spectacle.AST
  ( runAction,
    runInvariant,
    runTerminate,
  )
import Language.Spectacle.Exception (SpecException (ModelCheckerException, RuntimeException))
import Language.Spectacle.Exception.ModelCheckerException
  ( FormulaException (UnsatisfiedInvariant),
    ImpasseException (ImpasseInfiniteStutter, ImpasseNoTermination),
    ModelCheckerException
      ( FormulaException,
        ImpasseException
      ),
  )
import Language.Spectacle.Exception.RuntimeException (RuntimeException)
import Language.Spectacle.RTS.Registers (RuntimeState (newValues))
import Language.Spectacle.Spec.Base
  ( Fairness (StronglyFair, Unfair, WeaklyFair),
    Specifiable,
  )
import Language.Spectacle.Spec.Behavior (Behavior, occursInPrefix)
import Language.Spectacle.Spec.CheckResult
  ( CheckResult,
    isComplete,
    isSatisfied,
  )
import Language.Spectacle.Spec.Coverage
  ( HasCoverageMap (coverageMap),
    checksCompleted,
    emptyCoverageInfo,
    subsequentWorlds,
  )
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
import Language.Spectacle.Spec.Prop (interpretFormula)
import Language.Spectacle.Spec.Prop.Base
  ( PropCtx (PropCtx),
    PropState (PropState),
    infoHere,
    infoThere,
    runProp,
  )

-- ---------------------------------------------------------------------------------------------------------------------

stepModel :: forall ctx. Specifiable ctx => Rec ctx -> Model ctx [Behavior ctx]
stepModel world = do
  worldsThere <- takeQuotient world
  case worldsThere of
    Left exc -> do
      canTerminate <- checkTerminationWithoutFailure False
      if canTerminate
        then pure <$> view modelTrace
        else throwError exc
    Right worlds ->
      view modelFairness >>= \case
        Unfair -> stepUnfair world worlds
        WeaklyFair -> stepWeakFair world worlds
        StronglyFair -> stepStrongFair world worlds

stepUnfair :: Specifiable ctx => Rec ctx -> [Rec ctx] -> Model ctx [Behavior ctx]
stepUnfair world worldsThere = do
  -- Check the case in which we stutter infinitely: w -> w -> ... -> w
  _ <- checkInfiniteStutter world

  behavior <- view modelTrace

  if null worldsThere
    then pure <$> view modelTrace
    else do
      results <- forM worldsThere \there -> do
        checkStepInvariance True world there >>= \case
          Left exc -> throwFormulaException exc
          Right result
            | result ^. isSatisfied -> return (there, result)
            | otherwise -> throwFormulaException (UnsatisfiedInvariant world there)

      extensions <- forM results \(there, result) -> do
        stopHere <-
          use (coverageMap . to (HashMap.lookup there)) >>= \case
            Nothing -> return False
            Just info -> return (info ^. checksCompleted && result ^. isComplete)
        if stopHere
          then return [behavior :|> world]
          else do
            newContext <- asks \oldContext ->
              oldContext & modelTrace .~ (behavior :|> world)
            local (const newContext) (stepModel there)
      return (concat extensions)

stepWeakFair :: Specifiable ctx => Rec ctx -> [Rec ctx] -> Model ctx [Behavior ctx]
stepWeakFair world worldsThere = do
  behavior <- view modelTrace
  if null worldsThere
    then do
      canTerminate <- checkTerminationWithoutFailure False
      unless canTerminate (checkInfiniteStutter world)
      pure <$> view modelTrace
    else do
      results <- forM worldsThere \there -> do
        checkStepInvariance True world there >>= \case
          Left exc -> throwFormulaException exc
          Right result
            | result ^. isSatisfied -> return (there, result)
            | otherwise -> throwFormulaException (UnsatisfiedInvariant world there)
      extensions <- forM results \(there, result) -> do
        if result ^. isComplete
          then return [behavior :|> world]
          else do
            newContext <- asks \oldContext ->
              oldContext & modelTrace .~ (behavior :|> world)
            local (const newContext) (stepModel there)
      return (concat extensions)

stepStrongFair :: Specifiable ctx => Rec ctx -> [Rec ctx] -> Model ctx [Behavior ctx]
stepStrongFair world worldsThere = do
  behavior <- view modelTrace

  if null worldsThere
    then do
      canTerminate <- checkTerminationWithoutFailure False
      unless canTerminate (checkInfiniteStutter world)
      pure <$> view modelTrace
    else do
      results <- forM worldsThere \there -> do
        checkStepInvariance True world there >>= \case
          Left exc -> throwFormulaException exc
          Right result
            | result ^. isSatisfied -> return (there, result)
            | otherwise -> throwFormulaException (UnsatisfiedInvariant world there)
      extensions <- forM results \(there, result) -> do
        stopHere <-
          use (coverageMap . to (HashMap.lookup there)) >>= \case
            Nothing -> return False
            Just info -> return (info ^. checksCompleted && result ^. isComplete)
        if stopHere
          then return (Right [behavior :|> world])
          else do
            newContext <- asks \oldContext ->
              oldContext & modelTrace .~ (behavior :|> world)
            local (const newContext) (catchError (Right <$> stepModel there) (pure . Left))
      let extensions' = rights extensions
      if null extensions'
        then mapM throwError (lefts extensions)
        else return (concat extensions')

-- | Check if the model's invariant would be satisfied if an infinite sequence of stutter-steps extended model's
-- behavior.
--
-- @since 0.1.0.0
checkInfiniteStutter :: Specifiable ctx => Rec ctx -> Model ctx ()
checkInfiniteStutter world = do
  checkStutterInvariance world >>= \case
    Left exc -> throwFormulaException exc
    Right result
      | result ^. isSatisfied && result ^. isComplete -> return ()
      | otherwise -> throwImpasseException (ImpasseInfiniteStutter world)

checkStutterInvariance :: Specifiable ctx => Rec ctx -> Model ctx (Either FormulaException CheckResult)
checkStutterInvariance world =
  checkStepInvariance False world world
{-# INLINE checkStutterInvariance #-}

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
  let (result, propState') = runProp propState propCtx (interpretFormula terms)
  coverageMap %= HashMap.insert here (propState' ^. infoHere)
  coverageMap %= HashMap.insert there (propState' ^. infoThere)
  return result

checkTerminationWithoutFailure :: Specifiable ctx => Bool -> Model ctx Bool
checkTerminationWithoutFailure isEnabled =
  checkTermination isEnabled `catchError` const (return False)
{-# INLINE checkTerminationWithoutFailure #-}

checkTermination :: Specifiable ctx => Bool -> Model ctx Bool
checkTermination isEnabled = do
  here <- view worldHere
  view modelTerminate >>= \case
    Nothing -> throwImpasseException (ImpasseNoTermination here)
    Just terminate -> return (runTerminate isEnabled here terminate)

-- | Runs the next-state relation for this model to yield a set of worlds accessible from the given world.
--
-- @since 0.1.0.0
takeQuotient :: forall ctx. Specifiable ctx => Rec ctx -> Model ctx (Either SpecException [Rec ctx])
takeQuotient world =
  view (modelAction . to (runAction world)) >>= \case
    Left exc -> do
      behavior <- view modelTrace
      return (Left (RuntimeException behavior exc))
    Right xs -> do
      worlds <- Set.fromList <$> filterUnrelated xs
      coverageMap %= flip HashMap.adjust world \info ->
        info & subsequentWorlds <>~ worlds
      return (Right (Set.toList worlds))
  where
    filterUnrelated :: [(RuntimeState ctx, Bool)] -> Model ctx [Rec ctx]
    filterUnrelated [] = return []
    filterUnrelated ((rst, isRelated) : xs) = do
      let world' = newValues rst
      behavior <- view  modelTrace
      xs' <- filterUnrelated xs
      if world /= world' && not (world' `occursInPrefix` behavior) && isRelated
        then return (world' : xs')
        else return xs'

throwModelCheckerException :: Show (Rec ctx) => ModelCheckerException -> Model ctx a
throwModelCheckerException exc = do
  behavior <- view modelTrace
  throwError (ModelCheckerException behavior exc)
{-# INLINE throwModelCheckerException #-}

throwRuntimeException :: Show (Rec ctx) => RuntimeException -> Model ctx a
throwRuntimeException exc = do
  behavior <- view modelTrace
  throwError (RuntimeException behavior exc)
{-# INLINE throwRuntimeException #-}

throwFormulaException :: Show (Rec ctx) => FormulaException -> Model ctx a
throwFormulaException = throwModelCheckerException . FormulaException
{-# INLINE throwFormulaException #-}

throwImpasseException :: Show (Rec ctx) => ImpasseException -> Model ctx a
throwImpasseException = throwModelCheckerException . ImpasseException
{-# INLINE throwImpasseException #-}
