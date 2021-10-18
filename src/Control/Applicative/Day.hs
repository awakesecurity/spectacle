{-# LANGUAGE TupleSections #-}

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
    wrapDay,
  )
where

import Control.Applicative (liftA2)
import Data.Bifunctor (first)
import Data.Kind (Type)

-- ---------------------------------------------------------------------------------------------------------------------

newtype Day :: (Type -> Type) -> Type -> Type where
  Day :: {getDay :: forall x. f x -> f (a, x)} -> Day f a

wrapDay :: Monad m => m (Day m a) -> Day m a
wrapDay f = Day \x -> (($ x) . getDay) =<< f

-- | @since 0.1.0.0
instance Functor f => Functor (Day f) where
  fmap f (Day xs) = Day (fmap (first f) . xs)
  {-# INLINE fmap #-}

-- | @since 0.1.0.0
instance Functor f => Applicative (Day f) where
  pure x = Day (fmap (x,))
  {-# INLINE pure #-}

  liftA2 c (Day fs) (Day gs) = Day (fmap (\(x, (y, z)) -> (c x y, z)) . fs . gs)
  {-# INLINE liftA2 #-}
