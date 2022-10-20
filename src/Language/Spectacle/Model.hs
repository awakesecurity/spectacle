-- |
--
-- @since 1.0.0
module Language.Spectacle.Model where

import Control.Monad (unless)
import Control.Monad.Except (MonadError (throwError))
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Reader (local)
import Control.Monad.State (gets)
import Data.Foldable (for_, traverse_)
import Data.Function (on)
import Data.Hashable (Hashable)
import Data.List (nubBy)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Traversable (for)
import Lens.Micro (over, (&), (^.))
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
    valuation,
  )
import Language.Spectacle.Model.ModelState as ModelState
  ( enabledActionsAt,
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
          Eventually -> checkFuture formula modelTree
          Infinitely -> checkInfinitely formula modelTree
          Stays -> checkStays formula modelTree

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

checkStays :: Monad m => ModelTemporal ctx -> Tree (World ctx) -> ModelM ctx m ()
checkStays formula (root :- subtrees) = do
  results <- traverse (go root) subtrees
  let satisfied = and results
  unless satisfied do
    let name = modelTemporalName formula
    throwRefuteStays name (Just root) Nothing
  where
    go here (there :- nexts)
      | null nexts = do
        pure (getModelTemporal formula here there)
      | otherwise = do
        results <- traverse (go there) nexts
        pure (and results)

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
                  pure (world :- [world :- []])
                else do
                  subtrees <- for todoWF \actionWF ->
                    local (over weakFairActions (Set.delete actionWF)) do
                      entries <- use (indexNode hash . nextEntries actionWF)
                      traverse run (filter (/= hash) entries)
                  pure (world :- nubBy ((==) `on` rootOf) (concat subtrees))
            else do
              subtrees <- for todoSF \actionSF -> do
                local (over strongFairActions (Set.delete actionSF)) do
                  entries <- use (indexNode hash . nextEntries actionSF)
                  traverse run (filter (/= hash) entries)
              pure (world :- nubBy ((==) `on` rootOf) (concat subtrees))
        else do
          subtrees <- for actionsTodo \todo -> do
            entries <- use (indexNode hash . nextEntries todo)
            queuedActionsAt hash %= Set.delete todo
            -- Take only changing steps
            traverse run (filter (/= hash) entries)

          pure (world :- concat subtrees)

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
