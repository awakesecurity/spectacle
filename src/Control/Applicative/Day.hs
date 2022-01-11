{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeFamilies #-}

-- | Cayley applicative transformer.
--
-- === Reference
--
-- 1. <https://doisinkidney.com/posts/2020-11-23-applicative-queue.html>
--
-- 2. "Notions of Computations as Monoids" <https://arxiv.org/abs/1406.4823>
--
-- @since 0.1.0.0
module Control.Applicative.Day
  ( Day (Day),
    getDay,
    sec,
    rep,
    wrapDay,
  )
where

import Control.Applicative (liftA2)
import Data.Bifunctor (first)
import Data.Kind (Type)

-- ---------------------------------------------------------------------------------------------------------------------

newtype Day :: (Type -> Type) -> Type -> Type where
  Day :: {getDay :: forall x. f x -> f (a, x)} -> Day f a

sec :: Applicative f => f a -> Day f a
sec x = Day (liftA2 (,) x)

rep :: Applicative f => Day f a -> f a
rep (Day f) = fmap fst (f (pure ()))

wrapDay :: Monad m => m (Day m a) -> Day m a
wrapDay ma = Day \mx -> ma >>= \case
  Day k -> k mx

-- | @since 0.1.0.0
instance Functor f => Functor (Day f) where
  fmap f (Day xs) = Day (fmap (first f) . xs)
  {-# INLINE fmap #-}

-- | @since 0.1.0.0
instance Functor f => Applicative (Day f) where
  pure x = Day (fmap (x,))
  {-# INLINE pure #-}

  liftA2 c xs ys = Day (fmap (\(x,(y,z)) -> (c x y, z)) . getDay xs . getDay ys)
  {-# INLINE liftA2 #-}
