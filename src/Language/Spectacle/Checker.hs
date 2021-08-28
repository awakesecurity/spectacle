{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE UndecidableInstances #-}

module Language.Spectacle.Checker
  ( modelCheck,
  )
where

import Control.Monad.Except
  ( ExceptT,
    MonadError (throwError),
    runExcept,
  )
import Control.Monad.State.Strict ()
import Data.Either (partitionEithers)
import Data.Foldable (Foldable (fold))
import Data.Hashable (Hashable)
import Data.IntSet (IntSet)
import qualified Data.IntSet as IntSet
import qualified Data.Sequence as Seq
import Data.Set (Set)
import qualified Data.Set as Set
import Lens.Micro ((^.))
import Data.Tree

import Data.Type.Rec (Rec, ReflectRow)
import Language.Spectacle.AST (applyRewrites, runInitial, runInvariant)
import Language.Spectacle.Checker.CoverageMap (sizeCoverageMap)
import Language.Spectacle.Checker.Fingerprint (Fingerprint)
import Language.Spectacle.Checker.Metrics
  ( ModelMetrics (ModelMetrics),
  )
import Language.Spectacle.Checker.Model
import Language.Spectacle.Checker.Model.MCError (MCError (MCFormulaRuntimeError, MCInitialError, MCNoInitialStatesError))
import Language.Spectacle.Checker.Model.ModelEnv
  ( DisjunctZipper (LeftBranch, RightBranch),
    ModelEnv
      ( ModelEnv,
        _disjunctQueue,
        _livenessPropertyNames,
        _modelAction,
        _modelFairness,
        _modelFormula,
        _modelInitialWorld,
        _modelTerminate,
        _modelTrace,
        _srcLocMap
      ),
    makeDisjunctZips,
    makeSrcLocMap,
  )
import Language.Spectacle.Checker.Step (Step, makeReflexStep)
import Language.Spectacle.Checker.Universe (Universe, modelDepth, worldCoverage)
import Language.Spectacle.Checker.World
  ( World (World),
    newWorld,
    worldValues,
  )
import Language.Spectacle.Specification
  ( Specification
      ( Specification,
        fairnessConstraint,
        initialAction,
        nextAction,
        temporalFormula,
        terminationFormula
      ),
  )
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
import Language.Spectacle.Syntax.NonDet (foldMapA, oneOf)
import Data.Bag
import Debug.Trace
import qualified Data.Node as Bin

-- ---------------------------------------------------------------------------------------------------------------------

modelCheck ::
  forall ctx.
  (Hashable (Rec ctx), ReflectRow ctx, Show (Rec ctx)) =>
  Specification ctx ->
  Either [MCError ctx] ModelMetrics
modelCheck spec = do
  initialWorlds <- runExcept (runInitialAction spec)
  let checks :: [Either [MCError ctx] ModelMetrics]
      checks = foldMap (pure . runExcept . goModelCheck) initialWorlds
  case partitionEithers checks of
    ([], results) -> Right (fold results)
    (excs, _) -> Left (concat excs)
  where
    goModelCheck :: Monad m => World ctx -> ExceptT [MCError ctx] m ModelMetrics
    goModelCheck world = do
      path <- oneOf =<< disjunctionPaths spec (world ^. worldValues)
      env <- makeModelEnv spec world path
      let (result, universe) = runModel env mempty (stepInitial world)

      case result of
        Left excs -> throwError excs
        Right _ -> return (ModelMetrics (sizeCoverageMap (universe ^. worldCoverage)) (universe ^. modelDepth) 0)

runInitialAction ::
  (Monad m, Hashable (Rec ctx), ReflectRow ctx) =>
  Specification ctx ->
  ExceptT [MCError ctx] m (Set (World ctx))
runInitialAction Specification {..} =
  case runInitial initialAction of
    Left exc -> throwError [MCInitialError exc]
    Right states
      | null states -> throwError [MCNoInitialStatesError]
      | otherwise -> return (foldMap (Set.singleton . newWorld) states)
{-# INLINE runInitialAction #-}

disjunctionPaths ::
  (Monad m, Hashable (Rec ctx)) =>
  Specification ctx ->
  Rec ctx ->
  ExceptT [MCError ctx] m [[DisjunctZipper]]
disjunctionPaths Specification {..} world =
  case runInvariant True world world (applyRewrites temporalFormula) of
    Left exc -> throwError [MCFormulaRuntimeError (makeReflexStep (newWorld world)) exc]
    Right terms -> return (makeDisjunctZips terms)
{-# INLINE disjunctionPaths #-}

makeModelEnv ::
  forall ctx m.
  Monad m =>
  Specification ctx ->
  World ctx ->
  [DisjunctZipper] ->
  ExceptT [MCError ctx] m (ModelEnv ctx)
makeModelEnv Specification {..} initialWorld@(World _ values) disjuncts = do
  let formula = applyRewrites temporalFormula
  terms <- case runInvariant True values values formula of
    Left exc -> throwError [MCFormulaRuntimeError (makeReflexStep initialWorld) exc]
    Right terms -> return terms

  return
    ModelEnv
      { _modelFairness = fairnessConstraint
      , _modelAction = nextAction
      , _modelFormula = temporalFormula
      , _modelTerminate = terminationFormula
      , _modelTrace = Seq.empty
      , _modelInitialWorld = initialWorld
      , _livenessPropertyNames = getLivenessPropertyNames disjuncts terms
      , _disjunctQueue = disjuncts
      , _srcLocMap = makeSrcLocMap terms
      }
{-# INLINE makeModelEnv #-}

getLivenessPropertyNames :: [DisjunctZipper] -> Term Bool -> IntSet
getLivenessPropertyNames path = \case
  Value {} -> IntSet.empty
  Conjunct e1 e2 -> getLivenessPropertyNames path e1 <> getLivenessPropertyNames path e2
  Disjunct e1 e2
    | LeftBranch : path' <- path -> getLivenessPropertyNames path' e1
    | RightBranch : path' <- path -> getLivenessPropertyNames path' e2
    | otherwise -> undefined -- TODO
  Complement e -> getLivenessPropertyNames path e
  Always _ _ e -> getLivenessPropertyNames path e
  Eventually _ name e -> IntSet.singleton name <> getLivenessPropertyNames path e
  UpUntil _ name e1 e2 -> IntSet.singleton name <> getLivenessPropertyNames path e1 <> getLivenessPropertyNames path e2
  StaysAs _ _ e -> getLivenessPropertyNames path e
  InfinitelyOften _ _ e -> getLivenessPropertyNames path e
{-# INLINE getLivenessPropertyNames #-}
