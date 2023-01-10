{-# OPTIONS_HADDOCK show-extensions #-}

-- |
-- Module      :  Language.Spectacle.Model
-- Copyright   :  (c) Arista Networks, 2022-2023
-- License     :  Apache License 2.0, see LICENSE
--
-- Stability   :  stable
-- Portability :  non-portable (GHC extensions)
--
-- TODO: docs
--
-- @since 1.0.0
module Language.Spectacle.Model where

import Control.Monad (unless)
import Control.Monad.Except (MonadError (throwError))
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Reader (local)
import Control.Monad.State (gets, modify')
import Data.Foldable (for_, traverse_)
import Data.Function (on)
import Data.Hashable (Hashable)
import Data.List (nubBy)
import Data.Map qualified as Map
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Traversable (for)
import Lens.Micro (over, (&), (^.))
import Lens.Micro.Mtl (use, view, (%=), (.=))

import Data.Fingerprint (Fingerprint)
import Data.Foldable (fold)
import Data.Functor.Tree (Tree, rootOf, pattern (:-))
import Data.Map.Strict (Map)
import Data.Type.Rec (HasDict)
import Data.World (World (World), fingerprint, worldValues)
import Language.Spectacle.Model.ModelAction (
  ModelAction (modelActionName),
  fromActionSpec,
  runModelAction,
 )
import Language.Spectacle.Model.ModelError (
  ModelError (InitialError, RefutedError),
  TemporalError (TemporalError),
 )
import Language.Spectacle.Model.ModelNode as ModelNode (
  ModelNode (ModelNode),
  nextEntries,
  valuation,
 )
import Language.Spectacle.Model.ModelState as ModelState (
  enabledActionsAt,
  evidence,
  indexEvidence,
  indexNode,
  insertEvidence,
  member,
  queuedActionsAt,
 )
import Language.Spectacle.Model.ModelTemporal (
  ModelTemporal (getModelTemporal, modelTemporalName),
  fromTemporalSpec,
 )
import Language.Spectacle.Model.Monad (
  ModelM,
  modalityOf,
  newModelEnv,
  runModelM,
  strongFairActions,
  weakFairActions,
 )
import Language.Spectacle.Specification (
  Modality (Always, Eventually, Infinitely, Stays),
  Specification,
  getActionFormulae,
  getFairnessSpec,
  getModalitySpec,
  getTemporalFormulae,
  runInitialSpec,
 )
import qualified Debug.Trace as Debug

-- ---------------------------------------------------------------------------------------------------------------------

modelcheck ::
  (HasDict Eq ctx, HasDict Show ctx, HasDict Hashable ctx) =>
  Specification ctx acts form ->
  IO (Either (ModelError ctx) (Set (Tree (World ctx))))
modelcheck spec = do
  let initials = runInitialSpec spec
  let formulae = fromTemporalSpec (getTemporalFormulae spec)
  let actions = fromActionSpec (getActionFormulae spec)

  let env = newModelEnv (getFairnessSpec spec) (getModalitySpec spec)
  result <- snd <$> runModelM (checkModelSpec initials actions formulae) env mempty

  case result of
    Left err -> pure (Left err)
    Right modelTrees -> pure (Right modelTrees)

modeltrace ::
  (HasDict Eq ctx, HasDict Show ctx, HasDict Hashable ctx) =>
  Specification ctx acts form ->
  IO (Either (ModelError ctx) (Set (Tree (World ctx))))
modeltrace spec = do
  let initials = runInitialSpec spec
  let actions = fromActionSpec (getActionFormulae spec)

  let env = newModelEnv (getFairnessSpec spec) (getModalitySpec spec)
  snd <$> runModelM (traceModelSpec initials actions) env mempty

traceModelSpec ::
  (HasDict Show ctx, MonadIO m) =>
  Set (World ctx) ->
  [ModelAction ctx] ->
  ModelM ctx m (Set (Tree (World ctx)))
traceModelSpec initials actions
  | Set.null initials = throwError InitialError
  | otherwise = do
      _ <- unfoldModelState actions initials

      forSet initials \initial -> do
        runs <- expandAction initial
        let subtrees = Map.elems runs
        pure (initial :- subtrees)

checkModelSpec ::
  (HasDict Show ctx, MonadIO m) =>
  Set (World ctx) ->
  [ModelAction ctx] ->
  [ModelTemporal ctx] ->
  ModelM ctx m (Set (Tree (World ctx)))
checkModelSpec initials actions formulae
  | Set.null initials = throwError InitialError
  | otherwise = do
      _ <- unfoldModelState actions initials

      results <- forSet initials \initial -> do
        let hash = initial ^. fingerprint

        -- world <- World hash <$> use (indexNode hash . valuation)
        modelSubtrees <- expandAction initial 

        results <- for formulae \formula -> do
          let formulaName = modelTemporalName formula

          modality <- view (modalityOf formulaName)

          result <- flip Map.traverseWithKey modelSubtrees \action modelTree -> do

            result <- case modality of
              Always -> checkAlways formula modelTree
              Eventually -> checkFuture formula modelTree
              Infinitely -> checkInfinitely formula modelTree
              Stays -> checkStays formula modelTree

            modify' (insertEvidence hash action formulaName result)

            pure modelTree

          pure (Set.fromList (Map.elems result))

        pure (Set.unions results) -- modelTree
      
      pure (Set.unions results)

checkAlways ::
  forall m ctx.
  Monad m =>
  ModelTemporal ctx ->
  Tree (World ctx) ->
  ModelM ctx m Bool
checkAlways formula (initial :- subtrees) = do
  results <- traverse (go initial) subtrees
  pure (and results)
  where
    go :: World ctx -> Tree (World ctx) -> ModelM ctx m Bool
    go here (there :- nexts) = do
      let satisfied = getModelTemporal formula here there
      if satisfied
        then do
          results <- traverse (go there) nexts
          pure (and results)
        else do
          let name = modelTemporalName formula
          throwRefuteAlways name (Just here) (Just there)

checkFuture ::
  forall m ctx.
  MonadIO m =>
  ModelTemporal ctx ->
  Tree (World ctx) ->
  ModelM ctx m Bool
checkFuture formula (root :- subtrees) = do 
  results <- traverse (go root) subtrees
  pure (and results)
  where
    go :: World ctx -> Tree (World ctx) -> ModelM ctx m Bool
    go here (there :- nexts)
      | null nexts = do
          let name = modelTemporalName formula
          let satisfied = getModelTemporal formula here there
          if satisfied
            then do
              pure True
            else do
              throwRefuteEventually name (Just here) (Just there)
              -- pure False -- redundant but explicit
      | otherwise = do
          let satisfied = getModelTemporal formula here there
          if satisfied 
            then do 
              pure True
            else do 
              -- TODO: docs// tail behavior of satisfiability of <>.
              -- * ALL results should be true due to model_evidence
              -- * and doesn't make since here, but it seems more principled than or
              -- * any errors False cases will have thrown 'throwRefuteEventually' 
              results <- traverse (go there) nexts
              pure (and results)


checkInfinitely ::
  forall m ctx.
  Monad m =>
  ModelTemporal ctx ->
  Tree (World ctx) ->
  ModelM ctx m Bool
checkInfinitely formula (root :- subtrees) = do
  results <- traverse (go root) subtrees
  pure (and results)
  where
    go :: World ctx -> Tree (World ctx) -> ModelM ctx m Bool
    go here (there :- nexts)
      | null nexts = do
          let name = modelTemporalName formula
          throwRefuteInfinitely name (Just here) (Just there)
      | otherwise = do
          let satisfied = getModelTemporal formula here there
          if satisfied
            then do
              isCyclicallySatisfied <- and <$> traverse (cyclically here) nexts
              if isCyclicallySatisfied
                then do 
                  pure True 
                else do 
                  let name = modelTemporalName formula
                  throwRefuteInfinitely name (Just here) (Just there)
            else do 
              results <- traverse (go there) nexts
              pure (and results)

    cyclically match (here :- []) = pure (match == here)
    cyclically match (here :- there)
      | match == here = pure True
      | otherwise = and <$> traverse (cyclically match) there

-- Things to keep in mind:
--
-- 1. For any action A and for all states w_i and w_j.
--    if w_i == w_j ==> A(w_i) == A(w_j).

-- To detect a <>[](p) checking, with cycles for weak fairness:
--
-- 1. If we detect a state "w_i", such that @(<>[](p(w_i)) == 'False')@, then:
--   i.   We make a note of state w_i and continue checking.
--   ii.  If there exists some state "w_j", such that i < j and w_i == w_j,
--        then:
--     a. If theres some action A that has not been taken, we must take that
--        action.
--     b. If proceeding down the run A(w_j) does not imply @<>[](p(w_i))@. then
--        we may refute @<>[](p(w_i))@ or @<>[](p(w_j))@ (by w_i == w_j).
--   iii. Otherwise, if there does not exists some "w_j", such that i < j and
--        w_i == w_j, then:
--     a. If and only if. all actions A_n(w_i) do not return to w_i and do not
--        imply @<>[](p(w_i))@ can we refute that "stay-as" holds.
--

checkStays ::
  forall m ctx.
  MonadIO m =>
  ModelTemporal ctx ->
  Tree (World ctx) ->
  ModelM ctx m Bool
checkStays formula (root :- subtrees) = do
  results <- traverse (go root) subtrees
  let satisfied = and results
  if satisfied 
    then do 
      pure True
    else do
      let name = modelTemporalName formula
      throwRefuteStays name (Just root) Nothing
  where
    formulaName :: String
    formulaName = modelTemporalName formula

    go :: World ctx -> Tree (World ctx) -> ModelM ctx m Bool
    go here (there :- nexts) =
      if null nexts
        then do
          -- If there are no runs that have /new/ states that we can proceed in,
          -- then...
          -- pure (getModelTemporal formula here there)
          caseWeakFairPast here there
        else do
          results <- traverse (go there) nexts
          pure (and results)

    checkSatisfied ::
      -- \| The fingerprint of a world reachable by a model
      Fingerprint ->
      -- \| The name of a model action
      String ->
      ModelM ctx m Bool
    checkSatisfied hash action =
      gets (ModelState.indexEvidence hash action formulaName)

    -- @(actions :: Set String)@ be the set of actions that we can take, we
    -- partition @actions@ into two sets:
    --
    -- 1. The set of all actions that lead to runs which do satisfy <>[](p).
    --    We don't need to do anything for these actions, since we already know
    --    they can not be refuted.
    --
    -- 2. The set of all actions that lead to runs which do not satisfy <>[p].
    -- 
    -- NOTE: nexts == []
    caseWeakFairPast :: World ctx -> World ctx -> ModelM ctx m Bool
    caseWeakFairPast here there = do
      actionsWF <- view weakFairActions

      runResults <- forSet actionsWF \actionWF -> do
        isSatisfied <- checkSatisfied (here ^. fingerprint) actionWF

        if isSatisfied
          then pure True -- isSatisfied == True
          else checkSatisfied (there ^. fingerprint) actionWF

      pure (and runResults)

forSet :: (Applicative f, Ord b) => Set a -> (a -> f b) -> f (Set b)
forSet xs f = fmap Set.fromList . traverse f . Set.toList $ xs

expandAction ::
  forall m ctx.
  (HasDict Show ctx, MonadIO m) =>
  -- | TODO: docs
  World ctx ->
  -- | TODO: docs (actionName, Model run leading from @actionName@ of @hash@)
  ModelM ctx m (Map String (Tree (World ctx)))
expandAction = run
  where
    run :: World ctx -> ModelM ctx m (Map String (Tree (World ctx)))
    run world = do
      let fingerprintHere = world ^. fingerprint
      let !_ = Debug.trace ("fingerprint (world here): " ++ show world) ()

      enabled <- gets (enabledActionsAt fingerprintHere)
      actionsTodo <- do
        queued <- use (queuedActionsAt fingerprintHere)
        pure (filter (`Set.member` queued) enabled)

      if null actionsTodo
        then do
          actionsSF <- Set.toList <$> view strongFairActions
          let todoSF = filter (`elem` enabled) actionsSF

          if null todoSF
            then do
              actionsWF <- Set.toList <$> view weakFairActions
              let todoWF = filter (`elem` enabled) actionsWF

              if null todoWF
                then do
                  -- No actions left todo, no fairness constraints to solve, so stutter and terminate.
                  pure (foldr (`Map.insert` (world :- [])) Map.empty actionsWF)
                else do
                  subtrees <- for todoWF \actionWF ->
                    local (over weakFairActions (Set.delete actionWF)) do
                      entries <- use (indexNode fingerprintHere . nextEntries actionWF)

                      subtrees <- for (filter (/= fingerprintHere) entries) \fingerprintNext -> do 
                        world' <- World fingerprintNext <$> use (indexNode fingerprintNext . valuation)
                        result <- run world'
                        pure (Map.elems result)

                      pure (actionWF, world :- concat subtrees)

                  pure (Map.fromList subtrees)
            else do
              subtrees <- for todoSF \actionSF ->
                local (over strongFairActions (Set.delete actionSF)) do
                  entries <- use (indexNode fingerprintHere . nextEntries actionSF)
                  results <- undefined -- traverse run (filter (/= fingerprintHere) entries)
                  pure (actionSF, map Map.elems results)

              subtrees' <- for subtrees \(action, subtree) -> do
                let subtree' = undefined -- nubBy ((==) `on` rootOf) (concat (concat subtree))
                pure (action, subtree')

              pure undefined -- (Map.fromList subtrees')
        else do
          subtrees <- for actionsTodo \actionTodo -> do
            entries <- use (indexNode fingerprintHere . nextEntries actionTodo)

            let !_ = Debug.trace ("entries: " ++ show entries) ()

            queuedActionsAt fingerprintHere %= Set.delete actionTodo

            subtrees <- for (filter (/= fingerprintHere) entries) \fingerprintNext -> do 
              world' <- World fingerprintNext <$> use (indexNode fingerprintNext . valuation)
              fmap Map.elems (run world')
              
            let !_ = Debug.trace ("run results: " ++ show subtrees) ()

            pure (actionTodo, world :- concat subtrees)

          pure (Map.fromList subtrees)

unfoldModelState ::
  MonadIO m =>
  [ModelAction ctx] ->
  Set (World ctx) ->
  ModelM ctx m [Tree Fingerprint]
unfoldModelState actions = traverse go . Set.toList
  where
    go world = do
      let hash = world ^. fingerprint
      isSeen <- gets (ModelState.member hash)

      if isSeen
        then pure (hash :- [])
        else do
          nexts <- Map.fromList <$> traverse (runAction world) actions

          let entries = map (view fingerprint) <$> nexts
          let values = world ^. worldValues
          let queued =
                nexts & Map.foldMapWithKey \action worlds ->
                  if null worlds
                    then Set.empty
                    else Set.singleton action

          indexNode hash .= ModelNode entries queued values

          subtrees <- traverse go (concat (Map.elems nexts))
          pure (hash :- subtrees)

    runAction world action = do
      let name = modelActionName action
      worlds <- runModelAction world action
      pure (name, Set.toList worlds)

-- ---------------------------------------------------------------------------------------------------------------------

-- | 'throwRefute' is a helper combination for constructing an error refuting a temporal property and throwing it.
--
-- @since 1.0.0
throwRefute :: Monad m => Modality -> String -> Maybe (World ctx) -> Maybe (World ctx) -> ModelM ctx m a
throwRefute modality name here there = do
  let err = TemporalError modality name here there
  throwError (RefutedError err)

-- | Convinence function for constructing an "always" error and throwing it.
--
-- @since 1.0.0
throwRefuteAlways :: Monad m => String -> Maybe (World ctx) -> Maybe (World ctx) -> ModelM ctx m a
throwRefuteAlways = throwRefute Always

-- | Convinence function for constructing an "eventually" error and throwing it.
--
-- @since 1.0.0
throwRefuteEventually :: Monad m => String -> Maybe (World ctx) -> Maybe (World ctx) -> ModelM ctx m a
throwRefuteEventually = throwRefute Eventually

-- | Convinence function for constructing an "infinitely often" error and throwing it.
--
-- @since 1.0.0
throwRefuteInfinitely :: Monad m => String -> Maybe (World ctx) -> Maybe (World ctx) -> ModelM ctx m a
throwRefuteInfinitely = throwRefute Infinitely

-- | Convinence function for constructing an "stays as" error and throwing it.
--
-- @since 1.0.0
throwRefuteStays :: Monad m => String -> Maybe (World ctx) -> Maybe (World ctx) -> ModelM ctx m a
throwRefuteStays = throwRefute Stays
