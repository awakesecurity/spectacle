-- | 'Phases' applicative functor transformer.
--
-- === Reference
--
-- 1. <https://doisinkidney.com/posts/2020-11-23-applicative-queue.html>
--
-- 2. <https://github.com/rampion/tree-traversals>
--
-- @since 0.1.0.0
module Control.Applicative.Phases
  ( Phases (Here, There),
    lowerPhases,
    wrapPhases,
    liftPhases,
  )
where

import Control.Applicative ( Applicative(liftA2) )
import Data.Kind (Type)

-- ---------------------------------------------------------------------------------------------------------------------

-- | 'Phases' is similar to a free applicative functor with the primary differences being it is based on 'liftA2' rather
-- than '(<*>)' and its 'Applicative' instance.
--
-- The instance 'Applicative' of 'Phases' is not definitionally applicative, and is instead used to reorder and zip
-- effects of the underlying @f@. The 'Here' constructor apply effects immediately and 'There' constructors
-- incrementally delay effects.
--
-- @since 0.1.0.0
data Phases :: (Type -> Type) -> Type -> Type where
  Here :: a -> Phases f a
  There :: (a -> b -> c) -> f a -> Phases f b -> Phases f c

lowerPhases :: Applicative f => Phases f a -> f a
lowerPhases (Here x) = pure x
lowerPhases (There op x xs) = liftA2 op x (lowerPhases xs)

wrapPhases :: Monad f => f (Phases f a) -> Phases f a
wrapPhases f = There const (f >>= lowerPhases) (pure ())

liftPhases :: Monad f => Phases f (f a) -> Phases f a
liftPhases (Here x) = There const x (pure ())
liftPhases (There f x xs) = There const (x >>= \x' -> lowerPhases xs >>= f x') xs

-- | @since 0.1.0.0
instance Functor (Phases f) where
  fmap f (Here x) = Here (f x)
  fmap f (There op x xs) = There (\y ys -> f (y `op` ys)) x xs
  {-# INLINE fmap #-}

-- | @since 0.1.0.0
instance Applicative f => Applicative (Phases f) where
  pure = Here
  {-# INLINE pure #-}

  liftA2 c (Here x) ys = fmap (c x) ys
  liftA2 c xs (Here y) = fmap (`c` y) xs
  liftA2 c (There f x xs) (There g y ys) =
    There (\(x', y') (xs', ys') -> c (f x' xs') (g y' ys')) (liftA2 (,) x y) (liftA2 (,) xs ys)
  {-# INLINE liftA2 #-}
