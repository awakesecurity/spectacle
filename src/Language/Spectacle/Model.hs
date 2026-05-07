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
import Control.Monad.State (get, gets)
import Data.Foldable (for_, traverse_)
import Data.Function (on)
import Data.Hashable (Hashable)
import Data.List (nubBy)
import qualified Data.Map as Map
import Data.Maybe (listToMaybe, mapMaybe)
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Traversable (for)
import Lens.Micro ((&), (^.))
import Lens.Micro.Mtl (use, view, (%=), (.=))

import Data.Fingerprint (Fingerprint)
import Data.Functor.Tree (Tree, rootOf, pattern (:-))
import Data.Type.Rec (HasDict)
import Data.World (World (World), fingerprint, worldValues)
import Language.Spectacle.Model.ModelAction
  ( ModelAction (modelActionName),
    fromActionSpec,
    runModelAction,
  )
import Language.Spectacle.Model.ModelError
  ( ModelError (InitialError, RefutedError),
    TemporalError (TemporalError),
  )
import Language.Spectacle.Model.ModelNode as ModelNode
  ( ModelNode (ModelNode),
    nextEntries,
    nodeNextEntries,
    valuation,
  )
import Language.Spectacle.Model.ModelState as ModelState
  ( ModelState,
    enabledActionsAt,
    indexNode,
    member,
    queuedActionsAt,
  )
import Language.Spectacle.Model.ModelTemporal
  ( ModelTemporal (getModelTemporal, modelTemporalName),
    fromTemporalSpec,
  )
import Language.Spectacle.Model.Monad
  ( ModelM,
    modalityOf,
    newModelEnv,
    runModelM,
    strongFairActions,
    weakFairActions,
  )
import Language.Spectacle.Model.Scc
  ( SccInfo (..),
    computeSccs,
    isFairScc,
    reachableFrom,
    reachableVia,
    successorsOf,
  )
import Language.Spectacle.Specification
  ( Modality (Always, Eventually, Infinitely, Stays),
    Specification,
    getActionFormulae,
    getFairnessSpec,
    getModalitySpec,
    getTemporalFormulae,
    runInitialSpec,
  )

-- ---------------------------------------------------------------------------------------------------------------------

modelcheck ::
  (HasDict Eq ctx, HasDict Hashable ctx) =>
  Specification ctx acts form ->
  IO (Either (ModelError ctx) [Tree (World ctx)])
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
  (HasDict Eq ctx, HasDict Hashable ctx) =>
  Specification ctx acts form ->
  IO (Either (ModelError ctx) [Tree (World ctx)])
modeltrace spec = do
  let initials = runInitialSpec spec
  let actions = fromActionSpec (getActionFormulae spec)

  let env = newModelEnv (getFairnessSpec spec) (getModalitySpec spec)
  snd <$> runModelM (traceModelSpec initials actions) env mempty

traceModelSpec ::
  MonadIO m =>
  Set (World ctx) ->
  [ModelAction ctx] ->
  ModelM ctx m [Tree (World ctx)]
traceModelSpec initials actions
  | Set.null initials = do
    throwError InitialError
  | otherwise = do
    _ <- unfoldModelState actions initials

    for (Set.toList initials) \initial ->
      expandAction (initial ^. fingerprint)

checkModelSpec ::
  MonadIO m =>
  Set (World ctx) ->
  [ModelAction ctx] ->
  [ModelTemporal ctx] ->
  ModelM ctx m [Tree (World ctx)]
checkModelSpec initials actions formulae
  | Set.null initials = do
    throwError InitialError
  | otherwise = do
    _ <- unfoldModelState actions initials

    for (Set.toList initials) \initial -> do
      let hash = initial ^. fingerprint
      modelTree <- expandAction hash

      for_ formulae \formula -> do
        let name = modelTemporalName formula
        modality <- view (modalityOf name)
        case modality of
          Always -> checkAlways formula modelTree
          Eventually -> checkFutureScc formula initials
          Infinitely -> checkInfinitelyScc formula initials
          Stays -> checkStaysScc formula initials

      pure modelTree

checkAlways :: Monad m => ModelTemporal ctx -> Tree (World ctx) -> ModelM ctx m ()
checkAlways formula (initial :- subtrees) = traverse_ (go initial) subtrees
  where
    go here (there :- nexts) = do
      let satisfied = getModelTemporal formula here there
      unless satisfied do
        let name = modelTemporalName formula
        throwRefuteAlways name (Just here) (Just there)
      traverse_ (go there) nexts

checkFuture :: MonadIO m => ModelTemporal ctx -> Tree (World ctx) -> ModelM ctx m ()
checkFuture formula (root :- subtrees) = traverse_ (go root) subtrees
  where
    go here (there :- nexts)
      | null nexts = do
        let name = modelTemporalName formula
        let satisfied = getModelTemporal formula here there
        unless satisfied do
          throwRefuteEventually name (Just here) (Just there)
      | otherwise = do
        let satisfied = getModelTemporal formula here there
        unless satisfied do
          traverse_ (go there) nexts

checkInfinitely :: Monad m => ModelTemporal ctx -> Tree (World ctx) -> ModelM ctx m ()
checkInfinitely formula (root :- subtrees) = traverse_ (go root) subtrees
  where
    go here (there :- nexts)
      | null nexts = do
        let name = modelTemporalName formula
        throwRefuteInfinitely name (Just here) (Just there)
      | otherwise = do
        let satisfied = getModelTemporal formula here there
        if satisfied
          then do
            isCyclicallySatisfied <- and <$> traverse (cyclically here) nexts
            unless isCyclicallySatisfied do
              let name = modelTemporalName formula
              throwRefuteInfinitely name (Just here) (Just there)
          else do
            traverse_ (go there) nexts

    cyclically match (here :- []) = pure (match == here)
    cyclically match (here :- there)
      | match == here = pure True
      | otherwise = do
        and <$> traverse (cyclically match) there

-- | SCC-based @\<>[] P@ checker. The previous tree-walking @checkStays@ was
-- consulted on the (incorrectly fairness-annotated) tree from @expandAction@
-- and could not distinguish a real "stutter forever at @X@" behavior from a
-- genuine fair lasso, which is exactly what made issue #47's @Convergence@
-- spec refute spuriously.
--
-- The fair-cycle algorithm: @\<>[] P@ holds iff every fair lasso reaches a
-- bottom SCC ("BSCC") whose every visited transition satisfies @P@. Refute iff
-- some fair BSCC reachable from an initial state contains a transition whose
-- @getModelTemporal@ evaluates @False@ (or, for a sink BSCC — singleton without
-- a self-loop — its self-evaluation is @False@).
checkStaysScc ::
  MonadIO m =>
  ModelTemporal ctx ->
  Set (World ctx) ->
  ModelM ctx m ()
checkStaysScc formula initials = do
  mst <- get
  wf <- view weakFairActions
  sf <- view strongFairActions
  let initialHashes = Set.map (^. fingerprint) initials
      reach = reachableFrom mst initialHashes
      reachableSccs =
        filter (\info -> Set.isSubsetOf (sccNodes info) reach) (computeSccs mst)
      fairBsccs =
        filter (\info -> sccIsBottom info && isFairScc wf sf info) reachableSccs
      offenders = mapMaybe (badEdge mst formula) fairBsccs
  case listToMaybe offenders of
    Nothing -> pure ()
    Just (here, there) ->
      throwRefuteStays (modelTemporalName formula) (Just here) (Just there)

-- | Find a transition inside @info@ that does not satisfy @formula@. For a
-- trivial bottom SCC (sink) the check degenerates to the implicit stutter
-- @formula(X, X)@.
badEdge ::
  ModelState ctx ->
  ModelTemporal ctx ->
  SccInfo ->
  Maybe (World ctx, World ctx)
badEdge mst formula info
  | sccIsTrivial info = case Set.toList (sccNodes info) of
      [h] ->
        let w = mkWorld mst h
         in if getModelTemporal formula w w
              then Nothing
              else Just (w, w)
      _ -> Nothing
  | otherwise =
      listToMaybe
        [ (here, there)
        | h <- Set.toList (sccNodes info)
        , let here = mkWorld mst h
              entries = nodeNextEntries (mst ^. indexNode h)
        , (_action, targets) <- Map.toList entries
        , h' <- targets
        , h' `Set.member` sccNodes info
        , let there = mkWorld mst h'
        , not (getModelTemporal formula here there)
        ]

mkWorld :: ModelState ctx -> Fingerprint -> World ctx
mkWorld mst h = World h (mst ^. indexNode h . valuation)

-- | SCC-based @\<>P@ (Eventually) checker. A path satisfies @\<>P@ iff some
-- transition along it has @formula@ true (state-property formulas only depend
-- on the source, transition formulas can use both endpoints — the unified view
-- treats both via @getModelTemporal@). Refute iff there is a fair lasso whose
-- prefix is composed entirely of @¬P@-edges and whose ultimate BSCC has no
-- internal @P@-witness.
checkFutureScc ::
  MonadIO m =>
  ModelTemporal ctx ->
  Set (World ctx) ->
  ModelM ctx m ()
checkFutureScc formula initials = do
  mst <- get
  wf <- view weakFairActions
  sf <- view strongFairActions
  let initialHashes = Set.map (^. fingerprint) initials
      reach = reachableFrom mst initialHashes
      reachableSccs =
        filter (\info -> Set.isSubsetOf (sccNodes info) reach) (computeSccs mst)
      fairBsccs =
        filter (\info -> sccIsBottom info && isFairScc wf sf info) reachableSccs

      -- The path is @P@-free iff every traversed edge has @formula False@.
      isFreeEdge h h' =
        not (getModelTemporal formula (mkWorld mst h) (mkWorld mst h'))

      -- Frontier expansion only follows @¬P@-edges, modeling the "the path
      -- avoids @P@ all the way to the BSCC" condition.
      freeReach = reachableVia mst isFreeEdge initialHashes

      offendingBsccs =
        filter
          (\info -> isAllNotP mst formula info && not (Set.disjoint (sccNodes info) freeReach))
          fairBsccs
  case listToMaybe offendingBsccs of
    Nothing -> pure ()
    Just info -> case Set.toList (sccNodes info) of
      h : _ ->
        let w = mkWorld mst h
         in throwRefuteEventually (modelTemporalName formula) (Just w) Nothing
      _ -> pure ()

-- | SCC-based @[]\<>P@ (Infinitely) checker. A fair lasso satisfies @[]\<>P@
-- iff its BSCC contains a @P@-witness — some internal transition where
-- @formula@ holds, or for a sink the implicit stutter @formula(X, X)@. Refute
-- iff some fair BSCC reachable from initials has no @P@-witness.
checkInfinitelyScc ::
  MonadIO m =>
  ModelTemporal ctx ->
  Set (World ctx) ->
  ModelM ctx m ()
checkInfinitelyScc formula initials = do
  mst <- get
  wf <- view weakFairActions
  sf <- view strongFairActions
  let initialHashes = Set.map (^. fingerprint) initials
      reach = reachableFrom mst initialHashes
      reachableSccs =
        filter (\info -> Set.isSubsetOf (sccNodes info) reach) (computeSccs mst)
      fairBsccs =
        filter (\info -> sccIsBottom info && isFairScc wf sf info) reachableSccs

      offendingBsccs = filter (not . hasWitness mst formula) fairBsccs
  case listToMaybe offendingBsccs of
    Nothing -> pure ()
    Just info -> case Set.toList (sccNodes info) of
      h : _ ->
        let w = mkWorld mst h
         in throwRefuteInfinitely (modelTemporalName formula) (Just w) Nothing
      _ -> pure ()

-- | An @info@ BSCC has a @P@-witness when some internal edge satisfies the
-- formula or, for a trivial sink, the stutter self-evaluation does.
hasWitness :: ModelState ctx -> ModelTemporal ctx -> SccInfo -> Bool
hasWitness mst formula info
  | sccIsTrivial info = case Set.toList (sccNodes info) of
      [h] ->
        let w = mkWorld mst h
         in getModelTemporal formula w w
      _ -> False
  | otherwise =
      any
        ( \h ->
            let here = mkWorld mst h
             in any
                  ( \h' ->
                      h' `Set.member` sccNodes info
                        && getModelTemporal formula here (mkWorld mst h')
                  )
                  (successorsOf mst h)
        )
        (Set.toList (sccNodes info))

-- | An @info@ BSCC is "all @¬P@" when every internal transition fails the
-- formula and (for sinks) the stutter self-evaluation also fails.
isAllNotP :: ModelState ctx -> ModelTemporal ctx -> SccInfo -> Bool
isAllNotP mst formula info
  | sccIsTrivial info = case Set.toList (sccNodes info) of
      [h] ->
        let w = mkWorld mst h
         in not (getModelTemporal formula w w)
      _ -> False
  | otherwise =
      all
        ( \h ->
            let here = mkWorld mst h
             in all
                  ( \h' ->
                      not (h' `Set.member` sccNodes info)
                        || not (getModelTemporal formula here (mkWorld mst h'))
                  )
                  (successorsOf mst h)
        )
        (Set.toList (sccNodes info))

-- | Materialize the model as a 'Tree' of behaviors rooted at @hash@.
--
-- Termination uses the global per-state action queue from 'unfoldModelState':
-- each enabled action at a state is processed exactly once (via
-- @actionsTodo@) and removed from that state's queue. When a state is
-- revisited via a different path the queue is empty, and we emit a
-- @world :- [world :- []]@ stutter representing "we have already explored
-- from here." True terminal states (no enabled action) emit the same shape;
-- self-loop entries (an action that ran without changing state — see issue
-- #56) materialize as a @world :- []@ stutter leaf inside their parent so
-- terminal self-looping states appear as @here@ to some pair during property
-- checking.
--
-- Earlier formulations of @expandAction@ also @local@-deleted entries from
-- @weakFairActions@ \/ @strongFairActions@ on each recursive descent and
-- fell through to a terminal stutter once that env was empty. That
-- manufactured /false/ stutter leaves at non-terminal states (any state
-- whose queue had been consumed once and whose ancestors had already
-- discharged a WF\/SF action), which leaked into the tree-based liveness
-- checkers as fictitious "stutter forever at an enabled state" behaviors
-- (issues #47 and #56) and into @--trace --diagram@ output as misleading
-- paths. The fairness checkers (@checkFutureScc@, @checkInfinitelyScc@,
-- @checkStaysScc@) are now SCC-based and read 'ModelState' directly, so
-- the tree no longer needs to encode fairness obligations — only
-- @checkAlways@ and trace mode consume it, and both want a faithful
-- spanning of the reachable state graph.
expandAction :: MonadIO m => Fingerprint -> ModelM ctx m (Tree (World ctx))
expandAction = run
  where
    run hash = do
      world <- World hash <$> use (indexNode hash . valuation)

      enabled <- gets (enabledActionsAt hash)
      actionsTodo <- do
        queued <- use (queuedActionsAt hash)
        pure (filter (`Set.member` queued) enabled)

      if null actionsTodo
        then -- queue exhausted (revisit) or true terminal — emit a stutter
             pure (world :- [world :- []])
        else do
          subtrees <- for actionsTodo \action -> do
            entries <- use (indexNode hash . nextEntries action)
            queuedActionsAt hash %= Set.delete action
            children <- traverse run (filter (/= hash) entries)
            pure (if hash `elem` entries then (world :- []) : children else children)
          pure (world :- nubBy ((==) `on` rootOf) (concat subtrees))

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
        then do
          pure (hash :- [])
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
