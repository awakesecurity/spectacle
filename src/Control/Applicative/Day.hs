{-# LANGUAGE TupleSections #-}

-- | Cayley applicative transformer.
--
-- === Reference
--
-- 1. <https://doisinkidney.com/posts/2020-11-23-applicative-queue.html>
--
-- 2. "Notions of Computations as Monoids" <https://arxiv.org/abs/1406.4823>
--
-- @since 1.0.0
module Control.Applicative.Day
  ( Day (Day),
    getDay,
    wrapDay,
  )
where

import Control.Applicative (liftA2)
import Data.Bifunctor (first)
import Data.Kind (Type)

-- ---------------------------------------------------------------------------------------------------------------------

-- | Cayley applicative transformer.
--
-- @since 1.0.0
newtype Day :: (Type -> Type) -> Type -> Type where
  Day ::
    { -- | Retrieve the underlying @'Day'@ transformer
      --
      -- @since 1.0.0
      getDay :: forall x. f x -> f (a, x)
    } -> Day f a

-- | Wrap any @'Day'@ inside any given @'Monad'@.
wrapDay :: Monad m => m (Day m a) -> Day m a
wrapDay ma = Day \mx ->
  ma >>= \case
    Day k -> k mx

-- | @since 1.0.0
instance Functor f => Functor (Day f) where
  fmap f (Day xs) = Day (fmap (first f) . xs)
  {-# INLINE fmap #-}

-- | @since 1.0.0
instance Functor f => Applicative (Day f) where
  pure x = Day (fmap (x,))
  {-# INLINE pure #-}

  liftA2 c xs ys = Day (fmap (\(x, (y, z)) -> (c x y, z)) . getDay xs . getDay ys)
  {-# INLINE liftA2 #-}
