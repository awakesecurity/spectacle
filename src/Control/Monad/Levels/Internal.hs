{-# LANGUAGE TupleSections #-}
{-# LANGUAGE UndecidableInstances #-}

-- | The "Levels" search monad.
--
-- === Reference
--
-- 1. Donnacha Oisín Kidney, Nicolas Wu. 2021. Algebras for Weighted Search.
--
-- @since 0.1.0.0
module Control.Monad.Levels.Internal
  ( LevelsT (LevelsT, runLevelsT),
    liftLevelsT,
    wrapLevelsT,
  )
where

import Control.Applicative (Alternative (empty, (<|>)))
import Control.Monad (ap)
import Control.Monad.Except (MonadError (catchError, throwError))
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Reader (MonadReader (local, reader))
import Control.Monad.State (MonadState (state))
import Control.Monad.Trans.Class (MonadTrans, lift)
import Data.Kind (Type)

import Control.Hyper (HyperM (HyperM, invokeM))
import Data.Bag (Bag (None))
import qualified Data.Bag as Bag

-- ---------------------------------------------------------------------------------------------------------------------

newtype LevelsT :: (Type -> Type) -> Type -> Type where
  LevelsT :: {runLevelsT :: forall x. (Bag a -> m x -> m x) -> m x -> m x} -> LevelsT m a

liftLevelsT :: Monad m => m (LevelsT m a) -> LevelsT m a
liftLevelsT xs = LevelsT (\cons nil -> xs >>= \xs' -> runLevelsT xs' cons nil)
{-# INLINE liftLevelsT #-}

wrapLevelsT :: Monad m => m (LevelsT m a) -> LevelsT m a
wrapLevelsT xs = LevelsT (\cons nil -> cons None (xs >>= \xs' -> runLevelsT xs' cons nil))
{-# INLINE wrapLevelsT #-}

-- | @since 0.1.0.0
instance Functor (LevelsT m) where
  fmap f (LevelsT g) = LevelsT \cons nil -> g (cons . fmap f) nil
  {-# INLINE fmap #-}

-- | @since 0.1.0.0
instance Monad m => Applicative (LevelsT m) where
  pure x = LevelsT \cons nil -> cons (Bag.singleton x) nil
  {-# INLINE pure #-}

  -- TODO: Lower the definition of (<*>) from 'ap'.
  (<*>) = ap
  {-# INLINE (<*>) #-}

-- | @since 0.1.0.0
instance Monad m => Monad (LevelsT m) where
  LevelsT m >>= k = liftLevelsT (m (\x xs -> pure (foldr ((<|>) . k) (wrapLevelsT xs) x)) (pure empty))
  {-# INLINE (>>=) #-}

-- | @since 0.1.0.0
instance Monad m => Alternative (LevelsT m) where
  empty = LevelsT \_ nil -> nil
  {-# INLINE empty #-}

  LevelsT f <|> LevelsT g = LevelsT \cons nil ->
    let fcons x xs = pure (\k -> k (HyperM xs) x)
        fnil = pure \k -> k (HyperM fnil) None

        gcon y yk = pure \xk x -> cons (x <> y) (invokeM xk >>= (yk >>=))

        gnil _ None = nil
        gnil xk x = cons x (invokeM xk >>= ($ gnil))
     in f fcons fnil >>= (g gcon (pure gnil) >>=)
  {-# INLINE (<|>) #-}

-- | @since 0.1.0.0
instance MonadTrans LevelsT where
  lift m = LevelsT \cons nil -> m >>= (`cons` nil) . Bag.singleton
  {-# INLINE lift #-}

-- | @since 0.1.0.0
instance MonadState s m => MonadState s (LevelsT m) where
  state = lift . state
  {-# INLINE state #-}

-- | @since 0.1.0.0
instance MonadReader r m => MonadReader r (LevelsT m) where
  reader = lift . reader
  {-# INLINE reader #-}

  local f (LevelsT g) = LevelsT \cons nil ->
    local f (g cons nil)
  {-# INLINE local #-}

-- | @since 0.1.0.0
instance MonadError e m => MonadError e (LevelsT m) where
  throwError = lift . throwError
  {-# INLINE throwError #-}

  catchError (LevelsT f) g = LevelsT \cons nil ->
    catchError (f cons nil) (\e -> runLevelsT (g e) cons nil)
  {-# INLINE catchError #-}

-- | @since 0.1.0.0
instance MonadIO m => MonadIO (LevelsT m) where
  liftIO m = LevelsT \cons nil -> liftIO m >>= (`cons` nil) . Bag.singleton
  {-# INLINE liftIO #-}
