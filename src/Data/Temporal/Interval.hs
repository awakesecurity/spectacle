-- |
--
-- @since 0.1.0.0
module Data.Temporal.Interval
  ( -- * Semi-open time Intervals
    Interval (Interval),
    boundryElem,
  )
where

import Data.Bifunctor (Bifunctor (bimap))

import Data.Temporal.Time (Time (Inf, Time))

-- ---------------------------------------------------------------------------------------------------------------------

-- | 'Time' m n is a semi-open interval of time [m, n) ordered by a relation @r@.
--
-- @since 0.1.0.0
data Interval r a b
  = Interval
      {-# UNPACK #-} !r
      {-# UNPACK #-} !(Time a)
      {-# UNPACK #-} !(Time b)
  deriving (Eq, Functor, Show)

boundryElem :: r -> a -> Interval r a b
boundryElem r x = Interval r (Time x) Inf
{-# INLINE CONLIKE boundryElem #-}

-- | @since 0.1.0.0
instance Bifunctor (Interval r) where
  bimap f g (Interval r m n) = Interval r (fmap f m) (fmap g n)
  {-# INLINE bimap #-}
