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
    hoist,
    lower,
    lowerR,
    wrap,
    lift,
  )
where

import Control.Applicative (Applicative (liftA2))
import Control.Monad.Zip (MonadZip, mzipWith)
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

-- | @since 0.1.0.0
instance Functor (Phases f) where
  fmap f (Here x) = Here (f x)
  fmap f (There k x xs) = There (\y ys -> f (k y ys)) x xs
  {-# INLINE fmap #-}

-- | @since 0.1.0.0
instance Applicative f => Applicative (Phases f) where
  pure = Here
  {-# INLINE pure #-}

  liftA2 c (Here x) ys = fmap (c x) ys
  liftA2 c xs (Here y) = fmap (`c` y) xs
  liftA2 c (There f x xs) (There g y ys) =
    There
      (\(x,y) (xs,ys) -> c (f x xs) (g y ys))
      (liftA2 (,) x y)
      (liftA2 (,) xs ys)
  {-# INLINE liftA2 #-}

hoist :: (forall x. f x -> g x) -> Phases f a -> Phases g a
hoist _ (Here x) = Here x
hoist eta (There f x xs) = There f (eta x) (hoist eta xs)

lower :: Applicative f => Phases f a -> f a
lower (Here x) = pure x
lower (There op x xs) = liftA2 op x (lower xs)
{-# INLINE lower #-}

lowerR :: Applicative f => Phases f a -> f a
lowerR (Here x) = pure x
lowerR (There op x xs) = liftA2 (flip op) (lowerR xs) x

wrap :: Applicative f => Phases f a -> Phases f a
wrap = There (const id) (pure ())

lift :: Monad f => f (Phases f a) -> Phases f a
lift f = There const (f >>= lower) (pure ())
