{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Control.Monad.Fresh
  ( FreshT (FreshT, unFreshT),
    runFreshT,
    execFreshT,
    evalFreshT,
    Fresh,
    runFresh,
    execFresh,
    evalFresh,
    MonadFresh (fresh),
  )
where

import Control.Monad.Except (ExceptT)
import Control.Monad.Reader (ReaderT)
import Control.Monad.State
  ( MonadState (get, put),
    StateT (runStateT),
    evalState,
    evalStateT,
    execState,
    execStateT,
    runState,
  )
import Control.Monad.Trans (MonadTrans (lift))
import Data.Functor.Identity (Identity)

-- ---------------------------------------------------------------------------------------------------------------------

newtype FreshT m a = FreshT
  { unFreshT ::
      StateT Int m a
  }
  deriving (Functor, Applicative, Monad)
  deriving (MonadState Int)

runFreshT :: Int -> FreshT m a -> m (a, Int)
runFreshT n (FreshT st) = runStateT st n
{-# INLINE runFreshT #-}

execFreshT :: Monad m => Int -> FreshT m a -> m Int
execFreshT n (FreshT st) = execStateT st n
{-# INLINE execFreshT #-}

evalFreshT :: Monad m => Int -> FreshT m a -> m a
evalFreshT n (FreshT st) = evalStateT st n
{-# INLINE evalFreshT #-}

-- ---------------------------------------------------------------------------------------------------------------------

type Fresh = FreshT Identity

runFresh :: Int -> Fresh a -> (a, Int)
runFresh n (FreshT st) = runState st n
{-# INLINE runFresh #-}

execFresh :: Int -> Fresh a -> Int
execFresh n (FreshT st) = execState st n
{-# INLINE execFresh #-}

evalFresh :: Int -> Fresh a -> a
evalFresh n (FreshT st) = evalState st n
{-# INLINE evalFresh #-}

-- ---------------------------------------------------------------------------------------------------------------------

class Monad m => MonadFresh m where
  fresh :: m Int
  default fresh :: (MonadTrans t, MonadFresh n, m ~ t n) => m Int
  fresh = lift fresh

-- | @since 0.1.0.0
instance Monad m => MonadFresh (FreshT m) where
  fresh = do
    n <- get
    put (n + 1)
    return n
  {-# INLINE fresh #-}

-- | @since 0.1.0.0
instance MonadFresh m => MonadFresh (ExceptT e m)

-- | @since 0.1.0.0
instance MonadFresh m => MonadFresh (StateT r m)

-- | @since 0.1.0.0
instance MonadFresh m => MonadFresh (ReaderT r m)
