{-# LANGUAGE TupleSections #-}

-- | Mealy machines.
--
-- @since 1.0.0
module Control.Mealy
  ( -- * Mealy Machines
    Mealy,
    runMealy,

    -- ** Applicative Transformer
    MealyM (MealyM),
    runMealyM,
    arrM,
    refold,
  )
where

import Control.Applicative (liftA2)
import Data.Functor.Identity (Identity (runIdentity))

-- ---------------------------------------------------------------------------------------------------------------------

-- | A mealy machine @m@ over @a@ producing a value in @b@, as well as its continuation.
--
-- @since 1.0.0
newtype MealyM m a b = MealyM
  { -- | Run a mealy machine given an initial input value.
    runMealyM :: a -> m (b, MealyM m a b)
  }
  deriving (Functor)

-- | A pure mealy machine.
--
-- @since 1.0.0
type Mealy = MealyM Identity

-- | Run a @'Mealy'@ machine with an initial input.
--
-- @since 1.0.0
runMealy :: Mealy a b -> a -> (b, Mealy a b)
runMealy (MealyM k) x = runIdentity (k x)

-- | Lift any monadic function @a -> m b@ into a @'MealyM'@ machine.
--
-- @since 1.0.0
arrM :: Functor m => (a -> m b) -> MealyM m a b
arrM f = let k = MealyM (fmap (,k) . f) in k

-- | "Refold" the type of a mealy machine @a -> m b@ into a new type @a -> m c@.
--
-- @since 1.0.0
refold :: Monad m => (a -> b -> MealyM m a c -> m (c, MealyM m a c)) -> MealyM m a b -> MealyM m a c
refold f (MealyM k) =
  MealyM \s -> do
    (x, k') <- k s
    f s x (refold f k')

-- | @since 1.0.0
instance (Applicative m, Semigroup b) => Semigroup (MealyM m a b) where
  MealyM f <> MealyM g = MealyM \x -> liftA2 (<>) (f x) (g x)
  {-# INLINE (<>) #-}

-- | @since 1.0.0
instance (Applicative m, Monoid b) => Monoid (MealyM m a b) where
  mempty = MealyM \_ -> pure mempty
  {-# INLINE mempty #-}

-- | @since 1.0.0
instance Applicative m => Applicative (MealyM m a) where
  pure x = MealyM \_ -> pure (x, pure x)
  {-# INLINE pure #-}

  MealyM m <*> MealyM n = MealyM \x ->
    liftA2 (\(f, m') (c, n') -> (f c, m' <*> n')) (m x) (n x)
  {-# INLINE (<*>) #-}
