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
    lower,
    wrap,
    lift,
    tensor,
  )
where

import Control.Applicative ( Applicative(liftA2) )
import Data.Kind (Type)
import Control.Monad.Zip

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

lower :: Applicative f => Phases f a -> f a
lower (Here x) = pure x
lower (There op x xs) = liftA2 op x (lower xs)

wrap :: Applicative f => Phases f a -> Phases f a
wrap = There (\_ xs -> xs) (pure ())

lift :: Monad f => f (Phases f a) -> Phases f a
lift f = There const (f >>= lower) (pure ())

-- | Tensoring 'Phases' over 'MonadZip', preserves effect layering.
--
-- @since 0.1.0.0
tensor :: (MonadZip g, Monoid a, Applicative f) => Phases f (g a) -> Phases f (g a) -> Phases f (g a)
tensor (Here x) (Here y) = Here (mzipWith (<>) x y)
tensor (There f x xs) (Here y) = There (\z zs -> mzipWith (<>) (f z zs) y) x xs
tensor (Here x) (There g y ys) = There (\z zs -> mzipWith (<>) x (g z zs)) y ys
tensor (There f x xs) (There g y ys) =
  let here = liftA2 (,) x y
      there = liftA2 (,) xs ys
   in There (\(z, w) (zs, ws) -> mzipWith (<>) (f z zs) (g w ws)) here there

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

