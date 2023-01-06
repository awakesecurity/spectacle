{-# OPTIONS_HADDOCK show-extensions #-}

-- |
-- Module      :  Control.Monad.Ref
-- Copyright   :  (c) Arista Networks, 2022-2023
-- License     :  Apache License 2.0, see LICENSE
--
-- Stability   :  stable
-- Portability :  non-portable (GHC extensions)
--
-- State implemented over 'IORef'.
--
-- @since 1.0.0
module Control.Monad.Ref
  ( -- * RefM Transformer
    RefM (RefM),
    unRefM,

    -- ** Lowering
    runRefM,
    execRefM,
  )
where

import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.State (MonadState, get, put)
import Data.IORef (IORef, newIORef, readIORef, writeIORef)

-- ---------------------------------------------------------------------------------------------------------------------

-- | 'RefM' is an impure state monad transformer built around IORef.
--
-- In situations where the state @s@ is a large structure which undergoes frequent alteration, 'RefM' can be used as a
-- more preformant alternative to state (assuming its impurity is not a concern).
--
-- @since 1.0.0
newtype RefM s m a = RefM
  { -- | Acquire the underlying @'IORef'@ for a monadic stateful computation.
    unRefM :: IORef s -> m a
  }
  deriving (Functor)

-- | Run an impure reference-using computation, given a state, resulting in the
-- final state and return value.
--
-- @since 1.0.0
runRefM :: MonadIO m => RefM s m a -> s -> m (s, a)
runRefM (RefM k) st = do
  ref <- liftIO (newIORef st)
  ret <- k ref
  st' <- liftIO (readIORef ref)
  return (st', ret)

-- | Run an impure reference-using computation, given a state, resulting in the
-- final state.
--
-- @since 1.0.0
execRefM :: MonadIO m => RefM s m a -> s -> m s
execRefM refM st = fst <$> runRefM refM st

-- | @since 1.0.0
instance Applicative m => Applicative (RefM s m) where
  pure x = RefM \_ -> pure x
  {-# INLINE pure #-}

  RefM f <*> RefM m = RefM \ref ->
    f ref <*> m ref
  {-# INLINE (<*>) #-}

-- | @since 1.0.0
instance Monad m => Monad (RefM s m) where
  RefM f >>= m = RefM \ref ->
    f ref >>= \x -> unRefM (m x) ref
  {-# INLINE (>>=) #-}

-- | @since 1.0.0
instance MonadIO m => MonadIO (RefM s m) where
  liftIO m = RefM \_ -> liftIO m
  {-# INLINE liftIO #-}

-- | @since 1.0.0
instance MonadIO m => MonadState s (RefM s m) where
  get = RefM (liftIO . readIORef)
  {-# INLINE get #-}

  put x = RefM \ref -> liftIO (writeIORef ref x)
  {-# INLINE put #-}
