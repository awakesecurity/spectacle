{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE UndecidableInstances #-}

module Language.Spectacle.Spec
  ( Spec (..),
    SpecState (..),
    runSpec,
    newTargets,
    makeWorkers,
    spawnWorker,
  )
where

import Control.Concurrent.Async.Lifted.Safe
  ( Async,
    Forall,
    Pure,
    async,
    wait,
  )
import Control.Concurrent.STM.TMVar
  ( TMVar,
    newEmptyTMVar,
    putTMVar,
  )
import Control.Exception.Lifted (SomeException, evaluate)
import Control.Monad.Base (MonadBase, liftBase)
import Control.Monad.Catch (MonadCatch, MonadThrow, catch)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.State.Strict
  ( MonadState,
    StateT,
    evalStateT,
    gets,
  )
import Control.Monad.Trans (MonadTrans (lift))
import Control.Monad.Trans.Control (MonadBaseControl)
import Data.Functor ((<&>))
import GHC.Conc (atomically)

import Control.Monad.Mapping (MappingT, evalMappingT)
import Data.Type.HList (HList, HListT)
import qualified Data.Type.HList as HList

-- -----------------------------------------------------------------------------

-- | 'Spec' is a wrapper over state which can evaluate transition functions
-- while also handling the sharing of variables and exceptions thrown while
-- evaluating the transition.
--
-- @since 0.1.0.0
newtype Spec xs m r = Spec
  {unSpec :: StateT (SpecState xs) m r}
  deriving
    ( Functor
    , Applicative
    , Monad
    , MonadIO
    , MonadCatch
    , MonadThrow
    , MonadTrans
    , MonadState (SpecState xs)
    )

-- | The inner state used by 'Spec'.
--
-- @since 0.1.0.0
data SpecState xs = SpecState
  { vmStateVec :: HList xs
  , vmTargetsVec :: HListT TMVar xs
  }

-- | @since 0.1.0.0
deriving instance MonadBaseControl IO m => MonadBaseControl IO (Spec xs m)

-- | @since 0.1.0.0
deriving instance MonadBase IO m => MonadBase IO (Spec xs m)

-- | 'runSpec' runs the transition with the given initial state.
--
-- @since 0.1.0.0
runSpec ::
  (MonadBaseControl IO m, MonadCatch m, Forall (Pure m)) =>
  HList xs ->
  HListT (MappingT xs m) xs ->
  m (Either SomeException (HList xs))
runSpec initSt transition = do
  targetVec <- newTargets initSt
  evalStateT (unSpec $ evaluateTransition transition) $
    SpecState
      { vmStateVec = initSt
      , vmTargetsVec = targetVec
      }

-- | Construct a new target state from the initial state.
--
-- @since 0.1.0.0
newTargets :: MonadBase IO m => HList xs -> m (HListT TMVar xs)
newTargets xs = liftBase do
  atomically (HList.injectM (const newEmptyTMVar) xs)

-- | Evaluate a given transition with the initial state provided.
--
-- @since 0.1.0.0
evaluateTransition ::
  (MonadBaseControl IO m, MonadCatch m, Forall (Pure m)) =>
  HListT (MappingT xs m) xs ->
  Spec xs m (Either SomeException (HList xs))
evaluateTransition transition = do
  workers <- makeWorkers transition
  (lift (HList.surjectM wait workers) <&> Right)
    `catch` \err -> return (Left err)

-- | Spawn off a worker evaluating each map of a given transition in a seperate
-- thread.
--
-- @since 0.1.0.0
makeWorkers ::
  forall m xs.
  (MonadBaseControl IO m, Forall (Pure m)) =>
  HListT (MappingT xs m) xs ->
  Spec xs m (HListT Async xs)
makeWorkers transition = do
  stateVec <- gets vmStateVec
  targetVec <- gets vmTargetsVec
  lift (HList.zipWithM (spawnWorker stateVec targetVec) targetVec transition)

-- | Spawn a new worker thread evaluating the given mapping.
--
-- @since 0.1.0.0
spawnWorker ::
  (MonadBaseControl IO m, Forall (Pure m)) =>
  HList xs -> -- The initial state to evaluate the 'MappingT' with.
  HListT TMVar xs -> -- The scope of the 'MappingT'.
  TMVar r -> -- Where the 'MappingT' result will be stored.
  MappingT xs m r -> -- The map for the worker to evaluate.
  m (Async r)
spawnWorker initSt scope target mapping = async do
  result <- evalMappingT initSt scope mapping
  liftBase . atomically $ putTMVar target result
  evaluate result
