-- | Discrete time with infinity.
--
-- @since 0.1.0.0
module Data.Temporal.Time
  ( -- * Time
    Time (Time, Inf),
    timeExtract,
  )
where

-- ---------------------------------------------------------------------------------------------------------------------

-- | 'Time' is the disjoint union of a totally ordered @a@ with infinity.
--
-- @since 0.1.0.0
data Time a
  = Time {-# UNPACK #-} !a
  | Inf
  deriving (Eq, Functor, Show)

-- | Extracts the inner monoid of 'Time' sending 'Inf' to 'mempty'.
--
-- @since 0.1.0.0
timeExtract :: Monoid m => Time m -> m
timeExtract Inf = mempty
timeExtract (Time ms) = ms
{-# INLINE timeExtract #-}

-- | @since 0.1.0.0
instance Applicative Time where
  pure = Time
  {-# INLINE pure #-}

  Time f <*> x = fmap f x
  Inf <*> _ = Inf
  {-# INLINE (<*>) #-}

-- | @since 0.1.0.0
instance Monad Time where
  Time x >>= f = f x
  Inf >>= _ = Inf
  {-# INLINE (>>=) #-}

-- | Additive semigroup.
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
instance (Eq a, Enum a, Bounded a) => Enum (Time a) where
  toEnum n
    | n >= fromEnum (pred maxBound :: a) = Inf
    | n < fromEnum (minBound :: a) = Time minBound
    | otherwise = Time (toEnum n)
  {-# INLINE toEnum #-}

  fromEnum Inf = fromEnum (maxBound @a)
  fromEnum (Time n) = fromEnum n
  {-# INLINE fromEnum #-}

  succ Inf = Inf
  succ (Time n)
    | n == maxBound = Inf
    | otherwise = Time (succ n)
  {-# INLINE succ #-}

  pred Inf = Inf
  pred (Time n)
    | n == minBound = Time minBound
    | otherwise = Time (pred n)
  {-# INLINE pred #-}

-- | @since 0.1.0.0
instance Ord a => Ord (Time a) where
  compare Inf (Time _) = GT
  compare (Time _) Inf = LT
  compare Inf Inf = EQ
  compare (Time m) (Time n) = compare m n
  {-# INLINE compare #-}
