{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE UndecidableInstances #-}

module Language.Spectacle.Checker
  ( modelCheck,
  )
where

import Control.Applicative (Alternative ((<|>)))
import Control.Monad (forM)
import Control.Monad.Except (Except, MonadError (throwError), runExcept)
import Control.Monad.Reader (MonadReader, ReaderT (runReaderT))
import Control.Monad.State (MonadState, StateT, execStateT)
import Data.Coerce (coerce)
import qualified Data.Foldable as Foldable
import Data.Hashable (Hashable)
import qualified Data.IntMap.Strict as IntMap
import Data.IntSet (IntSet)
import qualified Data.IntSet as IntSet
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Lens.Micro ((&), (^.))
import Lens.Micro.Mtl (use, view, (%=), (.=))

import Control.Monad.Levels (LevelsT, foldMapAp, runLevelsA)
import Data.Type.Rec (Rec)
import Data.World (World (World), worldFingerprint)
import Language.Spectacle.Checker.Fingerprint (Fingerprint (Fingerprint))
import Language.Spectacle.Checker.Liveness (livenessCheck)
import qualified Language.Spectacle.Checker.MCCoverageMap as MCCoverageMap
import Language.Spectacle.Checker.MCEnv (MCEnv (MCEnv), mcEnvActionSpine, mcEnvPropInfo)
import Language.Spectacle.Checker.MCError (MCError (MCImpasseError))
import Language.Spectacle.Checker.MCMetrics (MCMetrics (MCMetrics), _distinctStates, _treeDepth, _treeWidth)
import Language.Spectacle.Checker.MCState
  ( MCState (MCState),
    mcStateCoverageMap,
    mcStateMaxDepth,
    mcStateMaxWidth,
  )
import Language.Spectacle.Checker.MCWorldInfo (MCWorldInfo (MCWorldInfo))
import Language.Spectacle.Checker.Model (modelNextSets)
import Language.Spectacle.Specification
  ( Spec (Spec),
    Specification,
    specInitialWorlds,
  )
import Language.Spectacle.Specification.Action
  ( ActionInfo,
    ActionSet (ActionSet, actionSetName, actionSetWorlds),
    ActionSpine,
    HasActions (ActionCtxt, takeActionSpine),
    spineToActionInfo,
  )
import Language.Spectacle.Specification.Prop
  ( HasProp (collectPropInfo),
    PropInfo (propInfoIsAlways, propInfoIsEventually),
  )
import Language.Spectacle.Specification.Variable (HasVariables (VariableCtxt))

-- ---------------------------------------------------------------------------------------------------------------------

newtype Model ctxt actions a where
  Model ::
    { runModel ::
        LevelsT (ReaderT (MCEnv ctxt actions) (StateT MCState (Except [MCError ctxt]))) a
    } ->
    Model ctxt actions a
  deriving
    ( Functor
    , Applicative
    , Monad
    , Alternative
    )
  deriving
    ( MonadReader (MCEnv ctxt actions)
    , MonadState MCState
    , MonadError [MCError ctxt]
    )

sendModel :: MCEnv ctxt acts -> Model ctxt acts a -> Either [MCError ctxt] MCState
sendModel env model =
  model
    & runModel
    & runLevelsA
    & flip runReaderT env
    & flip execStateT (MCState IntMap.empty mempty 0 0)
    & runExcept

modelCheck ::
  forall vars spec prop ctxt acts.
  ( Specification vars spec prop
  , VariableCtxt vars ~ ctxt
  , ActionCtxt ctxt spec ~ acts
  , Show (Rec ctxt)
  , Hashable (Rec ctxt)
  ) =>
  Spec vars spec prop ->
  Either [MCError ctxt] MCMetrics
modelCheck spec@(Spec _ sp) = do
  let initialWorlds :: Set (World ctxt)
      initialWorlds = specInitialWorlds spec

  mcFinalState <- sendModel mcenv (stepModel 0 initialWorlds)

  -- discard result, throws if liveness check fails
  _ <- livenessCheck
    (takeEventuallyActions propInfo)
    (Set.map (view worldFingerprint) initialWorlds)
    (mcFinalState ^. mcStateCoverageMap)
    actionInfo

  return
    MCMetrics
      { _distinctStates = MCCoverageMap.size (view mcStateCoverageMap mcFinalState)
      , _treeDepth = view mcStateMaxDepth mcFinalState
      , _treeWidth = view mcStateMaxWidth mcFinalState
      }
  where
    mcenv :: MCEnv ctxt acts
    mcenv = MCEnv actionInfo spine propInfo

    actionInfo :: Map String ActionInfo
    actionInfo = spineToActionInfo spine

    propInfo :: Map String PropInfo
    propInfo = collectPropInfo @prop

    spine :: ActionSpine ctxt acts
    spine = takeActionSpine sp

stepModel ::
  (Show (Rec ctxt), Hashable (Rec ctxt)) =>
  Int ->
  Set (World ctxt) ->
  Model ctxt acts (Set Fingerprint)
stepModel depth worldsHere
  | Set.null worldsHere = do
    stopModel depth
  | otherwise = do
    nextWorlds <- foldMapAp stepModelNext worldsHere
    let worldsThere = Set.difference nextWorlds worldsHere

    mcStateMaxWidth .= Set.size worldsThere

    pure (Set.map (view worldFingerprint) worldsHere) <|> stepModel (depth + 1) worldsThere

stopModel :: Int -> Model ctxt acts (Set a)
stopModel depth = do
  mcStateMaxDepth .= depth
  return Set.empty

stepModelNext :: Hashable (Rec ctxt) => World ctxt -> Model ctxt acts (Set (World ctxt))
stepModelNext worldHere@(World fingerprint _) = do
  explored <- MCCoverageMap.member fingerprint <$> use mcStateCoverageMap
  nextSets <- modelNextSets worldHere =<< view mcEnvActionSpine
  worldsThere <-
    forM nextSets \ActionSet {..} -> do
      nexts <- stepModelCheck actionSetName worldHere actionSetWorlds
      let worldInfo =
            if not (Set.null actionSetWorlds)
              then MCWorldInfo (foldr (takeEnabledActions actionSetName) Map.empty actionSetWorlds)
              else MCWorldInfo Map.empty

      mcStateCoverageMap %= MCCoverageMap.insert fingerprint worldInfo
      return nexts

  if explored
    then pure Set.empty
    else pure (Foldable.fold worldsThere)
  where
    takeEnabledActions :: String -> World ctxt -> Map String IntSet -> Map String IntSet
    takeEnabledActions action (World fpThere _) = flip Map.alter action \case
      Nothing -> Just (IntSet.singleton (fromIntegral fpThere))
      Just fpsThere -> Just (IntSet.insert (fromIntegral fpThere) fpsThere)

stepModelCheck ::
  String ->
  World ctxt ->
  Set (World ctxt) ->
  Model ctxt acts (Set (World ctxt))
stepModelCheck action worldHere worldsThere = do
  stepModelCheckAlways action worldHere worldsThere

stepModelCheckAlways ::
  String ->
  World ctxt ->
  Set (World ctxt) ->
  Model ctxt acts (Set (World ctxt))
stepModelCheckAlways action worldHere worldsThere = do
  isActionAlways <- maybe False propInfoIsAlways . Map.lookup action <$> view mcEnvPropInfo

  if isActionAlways && Set.null worldsThere
    then throwError [MCImpasseError worldHere]
    else do
      return worldsThere

takeEventuallyActions :: Map String PropInfo -> Set String
takeEventuallyActions = Map.foldrWithKey go Set.empty
  where
    go :: String -> PropInfo -> Set String -> Set String
    go action info xs
      | propInfoIsEventually info = Set.insert action xs
      | otherwise = xs
