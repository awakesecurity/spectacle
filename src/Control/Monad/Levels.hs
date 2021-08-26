-- | The "Levels" search monad.
--
-- === Reference
--
-- 1. Donnacha Oisín Kidney, Nicolas Wu. 2021. Algebras for Weighted Search.
--
-- @since 0.1.0.0
module Control.Monad.Levels
  ( LevelsT (LevelsT, runLevelsT),
    runLevelsA,
    liftLevelsT,
    wrapLevelsT,
  )
where

import Control.Applicative (Alternative (empty, (<|>)))
import Control.Hyper (HyperM (HyperM, invokeM))
import Control.Monad (ap)
import Control.Monad.Trans (MonadTrans (lift))
import Data.Kind (Type)

import Data.Bag (Bag (None))
import qualified Data.Bag as Bag

-- ---------------------------------------------------------------------------------------------------------------------

newtype LevelsT :: (Type -> Type) -> Type -> Type where
  LevelsT :: {runLevelsT :: forall x. (Bag a -> m x -> m x) -> m x -> m x} -> LevelsT m a

runLevelsA :: Alternative m => LevelsT m a -> m (Bag a)
runLevelsA (LevelsT m) = m ((<|>) . pure) (pure None)
{-# INLINE runLevelsA #-}

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
