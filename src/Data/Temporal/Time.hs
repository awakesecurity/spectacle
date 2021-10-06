{-# LANGUAGE GeneralizedNewtypeDeriving #-}

-- |
--
-- @since 0.1.0.0
module Data.Temporal.Time
  ( -- * Time
    Time (TimeInf),
    pattern Time,
    pattern Inf,
    timeExtract,

    -- * Intervals
    Interval (Interval),
    timeBefore,
    timeAfter,
  )
where

import Data.Bifunctor
import Data.Kind

-- ---------------------------------------------------------------------------------------------------------------------

-- | 'Time' is the disjoint union of a totally ordered @a@ with infinity.
--
-- @since 0.1.0.0
newtype Time :: Type -> Type where
  TimeInf :: {getTime :: Maybe a} -> Time a
  deriving (Applicative, Eq, Functor, Monad, Show)

pattern Time :: a -> Time a
pattern Time x = TimeInf (Just x)

pattern Inf :: Time a
pattern Inf = TimeInf Nothing

{-# COMPLETE Time, Inf #-}

-- | Extracts the inner monoid of 'Time' sending 'Inf' to 'mempty'.
--
-- @since 0.1.0.0
timeExtract :: Monoid m => Time m -> m
timeExtract Inf = mempty
timeExtract (Time ms) = ms
{-# INLINE timeExtract #-}

-- | 'Inf' annihilates.
--
-- @since 0.1.0.0
instance Semigroup a => Semigroup (Time a) where
  Inf <> _ = Inf
  _ <> Inf = Inf
  Time x <> Time y = Time (x <> y)
  {-# INLINE (<>) #-}

-- | @since 0.1.0.0
instance Monoid a => Monoid (Time a) where
  mempty = Time mempty
  {-# INLINE CONLIKE mempty #-}

-- | @since 0.1.0.0
instance Ord a => Ord (Time a) where
  compare Inf (Time _) = GT
  compare (Time _) Inf = LT
  compare Inf Inf = EQ
  compare (Time m) (Time n) = compare m n
  {-# INLINE compare #-}

-- ---------------------------------------------------------------------------------------------------------------------

data Interval a b = Interval
  { timeBefore :: a
  , timeAfter :: Time b
  }
  deriving (Eq, Ord, Functor, Show)

-- | @since 0.1.0.0
instance Bifunctor Interval where
  bimap f g (Interval t t') = Interval (f t) (fmap g t')
  {-# INLINE bimap #-}
