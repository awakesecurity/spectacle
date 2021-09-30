-- |
--
-- @since 0.1.0.0
module Data.Temporal.Family
  ( -- * Relations
    ARel (ARel),
    relation,
    canonical,
    representitive,
    equivalence,
    project,
    unitARel,
  )
where

import Control.Comonad (Comonad (duplicate, extend, extract))
import Data.Set (Set)
import qualified Data.Set as Set

import Data.Temporal.Time (Time (Time), timeExtract)

-- ---------------------------------------------------------------------------------------------------------------------

data ARel s i a = ARel
  { relation :: i
  , canonical :: Time s -> a
  , representitive :: Time s
  }

equivalence :: Time s -> ARel s i a -> a
equivalence s r = canonical r s
{-# INLINE equivalence #-}

project :: ARel s i a -> a
project (ARel _ f x) = f x
{-# INLINE project #-}

unitARel :: Ord a => i -> a -> ARel (Set a) i (Set a)
unitARel ix w = ARel ix timeExtract (Time (Set.singleton w))

-- | Action equality
--
-- @since 0.1.0.0
instance (Eq i, Eq s) => Eq (ARel s i a) where
  ARel ix1 _ x == ARel ix2 _ y = ix1 == ix2 && x == y
  {-# INLINE (==) #-}

-- | @since 0.1.0.0
instance Functor (ARel s i) where
  fmap f (ARel i g s) = ARel i (fmap f g) s
  {-# INLINE fmap #-}

-- | @since 0.1.0.0
instance Comonad (ARel s i) where
  duplicate (ARel i f s) = ARel i (ARel i f) s
  {-# INLINE duplicate #-}

  extend f (ARel i g s) = ARel i (f . ARel i g) s
  {-# INLINE extend #-}

  extract (ARel _ f s) = f s
  {-# INLINE extract #-}
