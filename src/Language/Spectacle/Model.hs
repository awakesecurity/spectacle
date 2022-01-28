{-# LANGUAGE Arrows #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}

-- |
--
-- @since 0.1.0.0
module Language.Spectacle.Model where

import Control.Applicative
import Control.Comonad.Cofree ()
import Control.Monad (filterM, foldM, unless, when)
import Control.Monad.Except (MonadError (throwError))
import Control.Monad.IO.Class (MonadIO (liftIO))
import Control.Monad.Reader (MonadReader, ReaderT (runReaderT), asks, local)
import Control.Monad.State (MonadState, execStateT, gets)
import Control.Monad.Trans.Except (ExceptT, runExceptT, throwE)
import Data.Foldable (for_, traverse_)
import Data.Function (on, (&))
import Data.Hashable (Hashable)
import Data.List (nubBy)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Traversable
import Lens.Micro (over, (^.))
import Lens.Micro.Mtl

import Data.Fingerprint
import Data.Functor.Tree (Tree, breadth, rootOf, pattern (:-))
import Data.Type.Rec (HasDict)
import Data.World (World (World), fingerprint, worldValues)
import qualified Debug.Trace as Debug
import Language.Spectacle.AST.Action
import Language.Spectacle.AST.Temporal
import Language.Spectacle.Fairness
import Language.Spectacle.Interaction.Diagram
import qualified Language.Spectacle.Interaction.Paths as Paths
import Language.Spectacle.Model.ModelAction
import Language.Spectacle.Model.ModelNode as ModelNode
import Language.Spectacle.Model.ModelState as ModelState
import Language.Spectacle.Model.ModelTemporal
import Language.Spectacle.Model.Monad as Model
import Language.Spectacle.Specification
import Prettyprinter.Render.Terminal (putDoc)

-- ---------------------------------------------------------------------------------------------------------------------

modelcheck ::
  (HasDict Hashable ctx, HasDict Show ctx) =>
  Specification ctx acts form ->
  IO (Either ModelError [Tree (World ctx)])
modelcheck spec = do
  let initials = runInitialSpec spec
  let formulae = fromTemporalSpec (getTemporalFormulae spec)
  let actions = fromActionSpec (getActionFormulae spec)

  let env = newModelEnv (getFairnessSpec spec) (getModalitySpec spec)
  (st, result) <- runModelM (checkModelSpec initials actions formulae) env

  case result of
    Left err -> pure (Left err)
    Right model -> do
      models <- snd <$> runModelM (checkModelSpec initials actions formulae) env
      case models of
        Left err -> pure (Left err)
        Right modelTrees -> do
          let points = foldMap Paths.toPointSet modelTrees
          let diagram = diagramFull points
          putDoc =<< runDiagram diagram

          pure (Right modelTrees)

checkModelSpec ::
  MonadIO m =>
  Set (World ctx) ->
  [ModelAction ctx] ->
  [ModelTemporal ctx] ->
  ModelM ctx m [Tree (World ctx)]
checkModelSpec initials actions formulae = do
  model <- unfoldModelState actions initials

  for (Set.toList initials) \initial -> do
    let hash = initial ^. fingerprint
    modelTree <- expandAction actions hash
    for_ formulae \formula -> do
      let name = modelTemporalName formula
      modality <- view (modalityOf name)
      case modality of
        Always -> checkAlways formula modelTree
        Eventually -> checkFuture formula modelTree
        Infinitely -> checkInfinitely formula modelTree
        Stays -> checkStays formula modelTree
    pure modelTree

checkAlways :: MonadIO m => ModelTemporal ctx -> Tree (World ctx) -> ModelM ctx m ()
checkAlways formula (initial :- subtrees) = traverse_ (go initial) subtrees
  where
    go here (there :- nexts) = do
      let satisfied = getModelTemporal formula here there
      unless satisfied do
        let name = modelTemporalName formula
        throwError (FailAlways name)

checkFuture :: MonadError ModelError m => ModelTemporal ctx -> Tree (World ctx) -> m ()
checkFuture formula (root :- subtrees) = traverse_ (go root) subtrees
  where
    go here (there :- nexts)
      | null nexts = do
        let name = modelTemporalName formula
        throwError (FailEventually name)
      | otherwise = do
        let satisfied = getModelTemporal formula here there
        unless satisfied do
          traverse_ (go there) nexts

checkInfinitely :: MonadError ModelError m => ModelTemporal ctx -> Tree (World ctx) -> m ()
checkInfinitely formula (root :- subtrees) = traverse_ (go root) subtrees
  where
    go here (there :- nexts)
      | null nexts = do
        let name = modelTemporalName formula
        throwError (FailInfinitely name)
      | otherwise = do
        let satisfied = getModelTemporal formula here there
        if satisfied
          then do
            isCyclicallySatisfied <- and <$> traverse (cyclically here) nexts
            unless isCyclicallySatisfied do
              let name = modelTemporalName formula
              throwError (FailInfinitely name)
          else do
            traverse_ (go there) nexts

    cyclically match (here :- []) = pure (match == here)
    cyclically match (here :- there)
      | match == here = pure True
      | otherwise = do
        and <$> traverse (cyclically match) there

checkStays :: MonadError ModelError m => ModelTemporal ctx -> Tree (World ctx) -> m ()
checkStays formula (root :- subtrees) = do
  results <- traverse (go root) subtrees
  let satisfied = and results
  unless satisfied do
    let name = modelTemporalName formula
    throwError (FailStays name)
  where
    go here (there :- nexts)
      | null nexts = do
        pure (getModelTemporal formula here there)
      | otherwise = do
        results <- traverse (go there) nexts
        pure (and results)

expandAction :: MonadIO m => [ModelAction ctx] -> Fingerprint -> ModelM ctx m (Tree (World ctx))
expandAction actions = run
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
