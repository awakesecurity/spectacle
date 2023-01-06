{-# OPTIONS_HADDOCK show-extensions #-}

-- |
-- Module      :  Control.Applicative.Phases
-- Copyright   :  (c) Arista Networks, 2022-2023
-- License     :  Apache License 2.0, see LICENSE
--
-- Stability   :  stable
-- Portability :  non-portable (GHC extensions)
--
-- 'Phases' applicative functor transformer.
--
-- The type can be explained by an effect system metaphor. A simple product type
-- @(,) :: Type -> Type -> Type@ can take two different ("heterogeneous") types
-- as parameters and stuff exactly two things inside. In contrast, a list type
-- @[] :: Type -> Type@ takes only one type, and can stuff an unbounded number
-- of things inside: the list is an iterative application of product type, where
-- the types parameters are equal ("homogenous").
--
-- Now, given the (covariant) Day Convolution (which is __NOT__ the same type
-- provided by this package via @'Control.Applicative.Day'@):
--
-- @
-- data Day f g a where
--   Day :: forall b. (a -> b -> c) -> f a -> g b -> Day f g c
-- @
--
-- we can think of @Day f g@ being an "enriched product type": one which is
-- designed for two homogenous effects types, @f@ and @g@. This leads us to a
-- metaphor:
--
-- "@Day@ is to @(,)@ what @'Phases'@ is to @[]@"": an iterative application of
-- a 2-ary type constructor, restricted to homogenous type parameters. Whereas
-- @Day@ allows us to carry exactly two heterogenously-typed effects and apply
-- them (and @(,)@ carries exactly two heterogenously-typed values), @'Phases'@
-- allows us to carry an unbounded number of homogenously-typed effects (nee,
-- unbounded number of homogenous-typed values) and apply them.
--
-- === References
--
-- 1. "A Queue for Effectful Breadth-First Traversals" 
--    <https://doisinkidney.com/posts/2020-11-23-applicative-queue.html>
-- 2. Tree Traversals
--    <https://github.com/rampion/tree-traversals>
-- 3. "Breadth-First Traversal Via Staging"
--    <https://www.cs.ox.ac.uk/people/jeremy.gibbons/publications/traversals.pdf>
--
-- @since 1.0.0
module Control.Applicative.Phases
  ( Phases (Here, There),
    lowerPhases,
    wrapPhases,
    liftPhases,
  )
where

import Control.Applicative (Applicative (liftA2))
import Data.Kind (Type)

-- ---------------------------------------------------------------------------------------------------------------------

-- | 'Phases' is similar to a free applicative functor with the primary differences being it is based on 'liftA2' rather
-- than '(<*>)' and its 'Applicative' instance.
--
-- The instance 'Applicative' of 'Phases' is not definitionally applicative, and is instead used to reorder and zip
-- effects of the underlying @f@. The 'Here' constructor apply effects immediately and 'There' constructors
-- incrementally delay effects.
--
-- @since 1.0.0
data Phases :: (Type -> Type) -> Type -> Type where
  Here :: a -> Phases f a
  There :: (a -> b -> c) -> f a -> Phases f b -> Phases f c

-- | Discharge the underlying series of effects.
--
-- @since 1.0.0
lowerPhases :: Applicative f => Phases f a -> f a
lowerPhases (Here x) = pure x
lowerPhases (There op x xs) = liftA2 op x (lowerPhases xs)

-- | Given an effect producing a series of effects, "collapse" the effectful types.
--
-- @since 1.0.0
wrapPhases :: Monad f => f (Phases f a) -> Phases f a
wrapPhases f = There const (f >>= lowerPhases) (pure ())

-- | Given a series of effects producing an effect, collapse the effectful types.
--
-- @since 1.0.0
liftPhases :: Monad f => Phases f (f a) -> Phases f a
liftPhases (Here x) = There const x (pure ())
liftPhases (There f x xs) = There const (x >>= \x' -> lowerPhases xs >>= f x') xs

-- | @since 1.0.0
instance Functor (Phases f) where
  fmap f (Here x) = Here (f x)
  fmap f (There op x xs) = There (\y ys -> f (y `op` ys)) x xs
  {-# INLINE fmap #-}

-- | @since 1.0.0
instance Applicative f => Applicative (Phases f) where
  pure = Here
  {-# INLINE pure #-}

  liftA2 c (Here x) ys = fmap (c x) ys
  liftA2 c xs (Here y) = fmap (`c` y) xs
  liftA2 c (There f x xs) (There g y ys) =
    There (\(x', y') (xs', ys') -> c (f x' xs') (g y' ys')) (liftA2 (,) x y) (liftA2 (,) xs ys)
  {-# INLINE liftA2 #-}
