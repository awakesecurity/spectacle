{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE RecordWildCards #-}

-- |
--
-- @since 0.1.0.0
module Language.Spectacle.Checker.Replayer
  ( -- *
    Replayer (Replayer),
    runReplayer,
    replayModelTrace,

    -- * Behavior
    Behavior (Behavior),
    behaviorDepth,
    behaviorActions,
  )
where

import Control.Applicative
import Control.Monad.Except
import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Writer
import Data.Coerce
import Data.Hashable
import Data.IntSet (IntSet)
import qualified Data.IntSet as IntSet
import Data.Kind
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Lens.Micro
import Lens.Micro.Mtl
import qualified Data.Foldable as Foldable

import Control.Monad.Levels
import Data.Context
import Data.Type.Rec
import Data.World
import Language.Spectacle.Checker.Fingerprint
import Language.Spectacle.Checker.MCError
import Language.Spectacle.Checker.Model
import Language.Spectacle.Exception.RuntimeException
import Language.Spectacle.Specification
import Language.Spectacle.Specification.Action
import Language.Spectacle.Specification.Variable

import qualified Debug.Trace as Debug

-- ---------------------------------------------------------------------------------------------------------------------

type Replayer :: Context -> [Type] -> Type -> Type
newtype Replayer ctxt acts a where
  Replayer ::
    { runReplayer ::
        LevelsT (ExceptT [MCError ctxt] (ReaderT (RPEnv ctxt acts) (State RPState))) a
    } ->
    Replayer ctxt acts a
  deriving
    ( Functor
    , Applicative
    , Monad
    , Alternative
    )
  deriving
    ( MonadReader (RPEnv ctxt acts)
    , MonadError [MCError ctxt]
    , MonadState RPState
    )

-- ---------------------------------------------------------------------------------------------------------------------

type RPEnv :: Context -> [Type] -> Type
data RPEnv ctxt acts = RPEnv
  { _rpEnvInitial :: Fingerprint
  , _rpEnvFinal :: Fingerprint
  , _rpEnvTargetDepth :: Int
  , _rpEnvActionSpine :: ActionSpine ctxt acts
  , _rpEnvActionInfo :: Map String ActionInfo
  }

rpEnvInitial :: SimpleGetter (RPEnv ctxt acts) Fingerprint
rpEnvInitial = to _rpEnvInitial
{-# INLINE rpEnvInitial #-}

rpEnvFinal :: SimpleGetter (RPEnv ctxt acts) Fingerprint
rpEnvFinal = to _rpEnvInitial
{-# INLINE rpEnvFinal #-}

rpEnvTargetDepth :: SimpleGetter (RPEnv ctxt acts) Int
rpEnvTargetDepth = to _rpEnvTargetDepth
{-# INLINE rpEnvTargetDepth #-}

rpEnvActionSpine :: SimpleGetter (RPEnv ctxt acts) (ActionSpine ctxt acts)
rpEnvActionSpine = to _rpEnvActionSpine
{-# INLINE rpEnvActionSpine #-}

rpEnvActionInfo :: SimpleGetter (RPEnv ctxt acts) (Map String ActionInfo)
rpEnvActionInfo = to _rpEnvActionInfo
{-# INLINE rpEnvActionInfo #-}

-- ---------------------------------------------------------------------------------------------------------------------

newtype RPState = RPState
  { _rpStateCoverage :: IntSet
  }
  deriving (Semigroup, Monoid) via IntSet

rpStateCoverage :: Lens' RPState IntSet
rpStateCoverage =
  lens
    _rpStateCoverage
    \RPState {..} x -> RPState {_rpStateCoverage = x `IntSet.union` _rpStateCoverage, ..}
{-# INLINE rpStateCoverage #-}

-- ---------------------------------------------------------------------------------------------------------------------

data Behavior = Behavior
  { behaviorDepth :: Int
  , behaviorActions :: Set String
  }
  deriving Show

-- | @since 0.1.0.0
instance Eq Behavior where
  Behavior _ xs == Behavior _ ys = xs == ys
  {-# INLINE (==) #-}

-- | @since 0.1.0.0
instance Ord Behavior where
  compare (Behavior x _) (Behavior y _) = compare x y
  {-# INLINE compare #-}

-- | @since 0.1.0.0
instance Semigroup Behavior where
  Behavior x xs <> Behavior y ys = Behavior (x `max` y) (xs `Set.union` ys)
  {-# INLINE (<>) #-}

-- | @since 0.1.0.0
instance Monoid Behavior where
  mempty = Behavior 0 Set.empty
  {-# INLINE mempty #-}

replayModelTrace ::
  forall vars spec prop ctxt acts.
  ( Specification vars spec prop
  , VariableCtxt vars ~ ctxt
  , ActionCtxt ctxt spec ~ acts
  , Hashable (Rec ctxt)
  , Show (Rec ctxt)
  ) =>
  Fingerprint ->
  Fingerprint ->
  Int ->
  Spec vars spec prop ->
  Either [MCError ctxt] (Set Behavior)
replayModelTrace initial target depth spec@(Spec _ sp) = do
  resultTrace <- foldMapAp (stepReplayer 0) initialWorlds
    & runReplayer
    & runLevelsA
    & runExceptT
    & flip runReaderT (RPEnv initial target depth spine (spineToActionInfo spine))
    & flip evalState mempty

  return (foldMap Set.singleton resultTrace)
  where
    initialWorlds :: Set (World ctxt)
    initialWorlds = specInitialWorlds spec

    spine :: ActionSpine ctxt acts
    spine = takeActionSpine sp

stepReplayer ::
  forall ctxt acts.
  (Show (Rec ctxt), Hashable (Rec ctxt)) =>
  Int ->
  World ctxt ->
  Replayer ctxt acts Behavior
stepReplayer depth worldHere@(World fingerprint _) = do
  finalTarget <- view rpEnvFinal
  depthTarget <- view rpEnvTargetDepth
  explored <- IntSet.member (coerce fingerprint) <$> use rpStateCoverage

  Debug.trace (show depth ++ ", " ++ show worldHere) (pure ())

  if explored && depthTarget < depth
    then empty
    else do
      actionInfo <- view rpEnvActionInfo
      actionSets <- modelNextSets worldHere =<< view rpEnvActionSpine

      rpStateCoverage %= IntSet.insert (coerce fingerprint)

      let fairNexts = flip foldMap actionSets \ActionSet {..} ->
            let fairness = maybe Unfair actionInfoFairness (Map.lookup actionSetName actionInfo)
            in if fairness == Unfair || Set.null actionSetWorlds
                 then Set.empty
                 else Set.singleton (actionSetName, actionSetWorlds)

          actions = if Set.null fairNexts
            then foldMap (\ActionSet {..} -> Set.singleton (actionSetName, actionSetWorlds)) actionSets
            else fairNexts

      Debug.trace (show depth ++ " " ++ show depthTarget ++ ", " ++ show actions) (pure ())

      forAp actions \(action, nextWorlds) -> do
        forAp nextWorlds \nextWorld -> do
          let actionHere = Behavior depth (Set.singleton action)

          if view worldFingerprint nextWorld == finalTarget && depth + 1 == depthTarget
            then return actionHere
            else return actionHere <|> stepReplayer (depth + 1) nextWorld
