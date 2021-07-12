module Language.Spectacle.Checker.Model
  ( -- * Model Monad Stack
    Model (Model),
    runModel,

    -- * Model Operations
    stepModel,
    unfairStep,
    weakFairStep,
    strongFairStep,
    nextStep,

    -- * Model Checking Operations
    checkStutter,
    checkInfiniteStutter,
    checkTermination,
    checkFormula,
    runFormulaTerms,
    interpretFormula,
    checkAlways,
    checkEventually,
    checkUpUntil,

    -- * Model Predicates
    isExploredWorld,
    isUnexploredWorld,
    isExploredFingerprint,
    isUnexploredFingerprint,
    satisfiesLiveness,

    -- * World Operations
    takeQuotientOf,
    propagateLiveness,

    -- * Errors
    cyclicLivenessErrors,
  )
where

import Control.Applicative (Alternative (empty))
import Control.Monad (forM, guard, join, unless, void, when)
import Control.Monad.Except (MonadError (catchError, throwError))
import Control.Monad.Reader (MonadReader (local))
import Data.Bifunctor (Bifunctor (first))
import Data.Hashable (Hashable)
import qualified Data.IntMap as IntMap
import qualified Data.IntSet as IntSet
import Data.List (nub)
import Data.Set (Set)
import qualified Data.Set as Set
import GHC.Stack (SrcLoc)
import Lens.Micro (set, to, (^.))
import Lens.Micro.Mtl (use, view, (%=), (+=), (.=), (<~))

import Data.Type.Rec (Rec)
import Language.Spectacle.AST (runAction, runInvariant, runTerminate)
import Language.Spectacle.Checker.Cover (hasBeenExplored, livenessProperties, succeedingWorlds)
import Language.Spectacle.Checker.CoverageMap (coverageInfo)
import Language.Spectacle.Checker.Fairness (Fairness (StrongFair, Unfair, WeakFair))
import Language.Spectacle.Checker.Fingerprint (Fingerprint)
import Language.Spectacle.Checker.Model.Internal (Model (Model), runModel)
import Language.Spectacle.Checker.Model.MCError
  ( InternalErrorKind (EmptyDisjunctQueueK),
    MCError
      ( MCActionError,
        MCFormulaError,
        MCFormulaRuntimeError,
        MCImpasseError,
        MCStutterError,
        MCInternalError
      ),
    PropertyKind (AlwaysPropK, EventuallyPropK, UpUntilPropK),
    StutterKind (FiniteStutterK, InfiniteStutterK),
  )
import Language.Spectacle.Checker.Model.ModelEnv
  ( DisjunctZipper (LeftBranch, RightBranch),
    disjunctQueue,
    livenessPropertyNames,
    modelAction,
    modelFairness,
    modelFormula,
    modelTerminate,
    srcLocMap,
  )
import Language.Spectacle.Checker.Step
  ( Step (Step),
    StepImage (StepImage),
    makeReflexStep,
    makeStep,
    stepFrom,
    stepTo,
    toStepImage,
  )
import Language.Spectacle.Checker.Truth (TruthTable (TruthTable), stepImageTruthTable, stepTruth)
import Language.Spectacle.Checker.Universe (modelDepth, truthCoverage, worldCoverage)
import Language.Spectacle.Checker.World (World (World), newWorld, worldFingerprint, worldValues)
import Language.Spectacle.RTS.Registers (newValues)
import Language.Spectacle.Syntax.Modal.Term
  ( Term
      ( Always,
        Complement,
        Conjunct,
        Disjunct,
        Eventually,
        InfinitelyOften,
        StaysAs,
        UpUntil,
        Value
      ),
  )
import Language.Spectacle.Syntax.NonDet (foldMapA)

-- ---------------------------------------------------------------------------------------------------------------------
-- Model Operations

-- | 'stepModel' is the main model checker loop.
--
-- @since 0.1.0.0
stepModel :: (Show (Rec ctx), Hashable (Rec ctx)) => World ctx -> Model ctx (World ctx)
stepModel worldHere = do
  step <- nextStep worldHere
  fairness <- view modelFairness

  worldThere <- case fairness of
    Unfair -> unfairStep step
    WeakFair -> weakFairStep step
    StrongFair -> strongFairStep step

  worldCoverage . coverageInfo (worldHere ^. worldFingerprint) . hasBeenExplored .= True

  canTerminate <- checkTermination True worldThere
  if canTerminate
    then return worldThere
    else stepModel worldThere

-- | Takes a single model step for a unfair process.
--
-- @since 0.1.0.0
unfairStep :: Step ctx -> Model ctx (World ctx)
unfairStep step@(Step worldHere worldThere) = do
  checkFormula step
  checkInfiniteStutter worldHere

  liveness <- weakLivenessCheck (worldThere ^. worldFingerprint)
  unless liveness (throwError =<< cyclicLivenessErrors step)

  return worldThere
{-# INLINE unfairStep #-}

-- | Takes a single model step under a weak fairness constraint.
--
-- @since 0.1.0.0
weakFairStep :: Step ctx -> Model ctx (World ctx)
weakFairStep step@(Step worldHere worldThere) = do
  checkFormula step
  checkStutter worldHere

  liveness <- weakLivenessCheck (worldThere ^. worldFingerprint)
  unless liveness (throwError =<< cyclicLivenessErrors step)

  return worldThere
{-# INLINE weakFairStep #-}

-- | Takes a single model step under a strong fairness constraint.
--
-- @since 0.1.0.0
strongFairStep :: Step ctx -> Model ctx (World ctx)
strongFairStep step = do
  checkFormula step
  checkStutter (step ^. stepFrom)
  return (step ^. stepTo)
{-# INLINE strongFairStep #-}

-- | Generates a model step to check from the world given.
--
-- @since 0.1.0.0
nextStep :: Hashable (Rec ctx) => World ctx -> Model ctx (Step ctx)
nextStep world =
  takeQuotientOf world >>= foldMapA \world' ->
    if world == world'
      then empty
      else return (makeStep world world')
{-# INLINE nextStep #-}

-- ---------------------------------------------------------------------------------------------------------------------
-- Model Checking Operations

-- | Checks if extending the current execution trace with some finite number of stutter-steps violates the temporal
-- formula.
--
-- @since 0.1.0.0
checkStutter :: World ctx -> Model ctx ()
checkStutter world = do
  result <- (checkFormula (makeReflexStep world) >> pure Nothing) `catchError` (pure . Just)
  case result of
    Nothing -> return ()
    Just errs -> do
      let annotated = map (stutterErrorAnnotation FiniteStutterK) errs
      throwError annotated
{-# INLINE checkStutter #-}

-- | Checks if extending the current execution trace with an infinite sequence of stutter-step violates the temporal
-- formula.
--
-- @since 0.1.0.0
checkInfiniteStutter :: World ctx -> Model ctx ()
checkInfiniteStutter world = do
  let stutter = makeReflexStep world
  checkFormula stutter
  satLiveness <- satisfiesLiveness (world ^. worldFingerprint)
  unless satLiveness do
    errors <- cyclicLivenessErrors stutter
    throwError (map (stutterErrorAnnotation InfiniteStutterK) errors)
{-# INLINE checkInfiniteStutter #-}

-- | Checks if a world meets the termination condition. If a termination condition was not specified then
-- 'checkTermination' is always 'False'.
--
-- @since 0.1.0.0
checkTermination :: Bool -> World ctx -> Model ctx Bool
checkTermination isEnabled (World fingerprint world) =
  view modelTerminate >>= \case
    Nothing -> return False
    Just terminate -> do
      satLiveness <- satisfiesLiveness fingerprint
      if satLiveness
        then return (runTerminate isEnabled world terminate)
        else return False
{-# INLINE checkTermination #-}

checkFormula :: Step ctx -> Model ctx ()
checkFormula step = do
  terms <- runFormulaTerms step
  void (interpretFormula step terms)
  propagateLiveness (toStepImage step)
  return ()
{-# INLINE checkFormula #-}

-- | Produces the 'Term' tree of the models temporal formula.
--
-- @since 0.1.0.0
runFormulaTerms :: Step ctx -> Model ctx (Term Bool)
runFormulaTerms step@(Step worldHere worldThere) = do
  formula <- view modelFormula
  case runInvariant True (worldHere ^. worldValues) (worldThere ^. worldValues) formula of
    Left exc -> throwError [MCFormulaRuntimeError step exc]
    Right terms -> return terms
{-# INLINE runFormulaTerms #-}

-- | 'interpretFormula' evaluates a 'Term' tree of the model's temporal formula at the given step to verify that the
-- step does not violate the properties specified.
--
-- @since 0.1.0.0
interpretFormula :: Step ctx -> Term Bool -> Model ctx Bool
interpretFormula step = \case
  Value x -> return x
  Conjunct e1 e2 -> do
    result1 <- interpretFormula step e1
    result2 <- interpretFormula step e2
    return (result1 && result2)
  Disjunct e1 e2 ->
    view disjunctQueue >>= \case
      [] -> throwError [MCInternalError EmptyDisjunctQueueK]
      LeftBranch : xs -> local (set disjunctQueue xs) (interpretFormula step e1)
      RightBranch : xs -> local (set disjunctQueue xs) (interpretFormula step e2)
  Complement e -> not <$> interpretFormula step e
  Always srcLoc name e -> checkAlways step name srcLoc e
  Eventually _ name e -> checkEventually step name e
  UpUntil srcLoc name e1 e2 -> checkUpUntil step name srcLoc e1 e2
  StaysAs _ name e -> do
    result <- interpretFormula step e
    truthCoverage . stepTruth step name .= result
    return result
  InfinitelyOften _ name e -> do
    result <- interpretFormula step e
    truthCoverage . stepTruth step name .= result
    return result
{-# INLINE interpretFormula #-}

-- | @'checkAlways' step name srcLoc e@ checks a formula @always e@ identified by @name@ for the model-step @step@.
--
-- @since 0.1.0.0
checkAlways :: forall ctx. Step ctx -> Int -> Maybe SrcLoc -> Term Bool -> Model ctx Bool
checkAlways step name srcLoc e = do
  result <- interpretFormula step e
  truthCoverage . stepTruth step name .= result

  unless result (throwError [MCFormulaError step srcLoc AlwaysPropK])

  return result
{-# INLINE checkAlways #-}

-- | @'checkEventually' step name e@ checks a formula @eventually e@ identified by @name@ for the model-step @step@.
--
-- @since 0.1.0.0
checkEventually :: Step ctx -> Int -> Term Bool -> Model ctx Bool
checkEventually step@(Step worldHere worldThere) name e = do
  result <- interpretFormula step e
  truthCoverage . stepTruth step name .= result

  when result do
    worldCoverage . coverageInfo (worldHere ^. worldFingerprint) . livenessProperties %= IntSet.insert name
    worldCoverage . coverageInfo (worldThere ^. worldFingerprint) . livenessProperties %= IntSet.insert name

  return True
{-# INLINE checkEventually #-}

-- | @'checkUpUntil' step name srcLoc e1 e2@ checks a formula @upUntil e1 e2@ identified by @name@ for the model-step
-- @step@.
--
-- @since 0.1.0.0
checkUpUntil :: Step ctx -> Int -> Maybe SrcLoc -> Term Bool -> Term Bool -> Model ctx Bool
checkUpUntil step@(Step worldHere worldThere) name srcLoc e1 e2 = do
  result1 <- interpretFormula step e1
  result2 <- interpretFormula step e2
  truthCoverage . stepTruth step name .= result2

  unless (result1 || result2) (throwError [MCFormulaError step srcLoc UpUntilPropK])

  when result2 do
    worldCoverage . coverageInfo (worldHere ^. worldFingerprint) . livenessProperties %= IntSet.insert name
    worldCoverage . coverageInfo (worldThere ^. worldFingerprint) . livenessProperties %= IntSet.insert name

  return (result1 || result2)
{-# INLINE checkUpUntil #-}

-- | Preforms a liveness check under a weak-fairness constraint.
--
-- @since 0.1.0.0
weakLivenessCheck :: Fingerprint -> Model ctx Bool
weakLivenessCheck fingerprint = do
  explored <- isExploredFingerprint fingerprint
  if explored
    then satisfiesLiveness fingerprint
    else return True
{-# INLINE weakLivenessCheck #-}

-- ---------------------------------------------------------------------------------------------------------------------
-- Model Predicates

-- | A predicate for if the given world has been explored by the model checker.
--
-- @since 0.1.0.0
isExploredWorld :: World ctx -> Model ctx Bool
isExploredWorld world = isExploredFingerprint (world ^. worldFingerprint)
{-# INLINE isExploredWorld #-}

-- | A predicate for if the given world has not yet been explored.
--
-- @since 0.1.0.0
isUnexploredWorld :: World ctx -> Model ctx Bool
isUnexploredWorld = fmap not . isExploredWorld
{-# INLINE isUnexploredWorld #-}

-- | A predicate for if the given fingerprint has been explored.
--
-- @since 0.1.0.0
isExploredFingerprint :: Fingerprint -> Model ctx Bool
isExploredFingerprint fingerprint = use (worldCoverage . coverageInfo fingerprint . hasBeenExplored)
{-# INLINE isExploredFingerprint #-}

-- | A predicate for if the given fingerprint has not yet been checked.
--
-- @since 0.1.0.0
isUnexploredFingerprint :: Fingerprint -> Model ctx Bool
isUnexploredFingerprint = fmap not . isExploredFingerprint
{-# INLINE isUnexploredFingerprint #-}

-- | A predicate for if all liveness properties have been satisfied so far for the given fingerprint.
--
-- @since 0.1.0.0
satisfiesLiveness :: Fingerprint -> Model ctx Bool
satisfiesLiveness fingerprint = do
  allLivenessProps <- view livenessPropertyNames
  satLivenessProps <- use (worldCoverage . coverageInfo fingerprint . livenessProperties)
  return (allLivenessProps == satLivenessProps)
{-# INLINE satisfiesLiveness #-}

-- ---------------------------------------------------------------------------------------------------------------------
-- World Operations

-- | @'takeQuotientOf' w@ for some world @w@ evaluates the next-state relation at @w@. Producing either a set of new
-- states related by the action to explore or exceptions if the model is at an impasse.
--
-- @since 0.1.0.0
takeQuotientOf :: forall ctx. Hashable (Rec ctx) => World ctx -> Model ctx (Set (World ctx))
takeQuotientOf world@(World fingerprint values) = do
  guard =<< isUnexploredFingerprint fingerprint
  action <- view modelAction
  case runAction values action of
    Left exc -> throwError [MCActionError world exc]
    Right results
      | null results -> throwError [MCImpasseError world]
      | otherwise -> do
        let newWorlds = foldMap (uncurry dropUnrelated . first newValues) results
        worldCoverage . coverageInfo fingerprint . succeedingWorlds .= Set.map (^. worldFingerprint) newWorlds
        modelDepth += 1
        return newWorlds
  where
    dropUnrelated :: Rec ctx -> Bool -> Set (World ctx)
    dropUnrelated world' isRelated
      | isRelated = Set.singleton (newWorld world')
      | otherwise = Set.empty
{-# INLINE takeQuotientOf #-}

-- | If @s@ is the given 'StepImage', @'propagateLiveness' s@ updates the coverage information such that all liveness
-- properties satisfied by the initial world in step are also satisfied by the terminal world in the step. The
-- implication being if @eventually p@ is true for a world @w_n@, then it follows that @eventually p@ is also true for
-- a world @w_n+1@.
--
-- @since 0.1.0.0
propagateLiveness :: StepImage -> Model ctx ()
propagateLiveness step@(StepImage fromWorld toWorld) = do
  worldCoverage . coverageInfo toWorld . livenessProperties
    <~ use (worldCoverage . coverageInfo fromWorld . livenessProperties)

  truthCoverage . stepImageTruthTable step <~ do
    livenessProps <- use (worldCoverage . coverageInfo fromWorld . livenessProperties)
    let table = IntSet.foldr (`IntMap.insert` True) IntMap.empty livenessProps
    return (TruthTable table)
{-# INLINE propagateLiveness #-}

-- | Retrieves a set of liveness properties that have not been satisfied by the given step.
--
-- @since 0.1.0.0
getUnsatisfiedLiveness :: Step ctx -> Model ctx [Int]
getUnsatisfiedLiveness step = do
  allProps <- view livenessPropertyNames
  nub . concat <$> forM (IntSet.toList allProps) \name -> do
    truth <- use (truthCoverage . stepTruth step name)
    if truth
      then return []
      else return [name]
{-# INLINE getUnsatisfiedLiveness #-}

-- ---------------------------------------------------------------------------------------------------------------------
-- Model Errors

-- | Annotate a 'MCFormulaError' as a formula violation that occured for a stuttering step.
--
-- @since 0.1.0.0
stutterErrorAnnotation :: StutterKind -> MCError ctx -> MCError ctx
stutterErrorAnnotation stutterK = \case
  MCFormulaError step srcLoc propK -> MCStutterError step srcLoc propK stutterK
  mcError -> mcError
{-# INLINE stutterErrorAnnotation #-}

-- | Produce errors for all liveness properties that are unsatisfied within some transitively closed cycle of a models
-- state graph.
--
-- @since 0.1.0.0
cyclicLivenessErrors :: forall ctx. Step ctx -> Model ctx [MCError ctx]
cyclicLivenessErrors step = do
  unsatProps <- getUnsatisfiedLiveness step
  forM unsatProps \prop -> do
    srcLoc <- view (srcLocMap . to (join . IntMap.lookup prop))
    return (MCFormulaError step srcLoc EventuallyPropK)
{-# INLINE cyclicLivenessErrors #-}
