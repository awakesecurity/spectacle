{-# LANGUAGE TupleSections #-}

-- |
--
-- @since 0.1.0.0
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
import Control.Monad ((>=>))
import Data.Bifunctor (second)
import Data.Functor.Identity

-- ---------------------------------------------------------------------------------------------------------------------

newtype MealyM m a b = MealyM
  {runMealyM :: a -> m (b, MealyM m a b)}
  deriving (Functor)

arrM :: Functor m => (a -> m b) -> MealyM m a b
arrM f = let k = MealyM (fmap (,k) . f) in k

refold :: Monad m => (a -> b -> MealyM m a c -> m (c, MealyM m a c)) -> MealyM m a b -> MealyM m a c
refold f (MealyM k) =
  MealyM \s -> do
    (x, k') <- k s
    f s x (refold f k')

type Mealy = MealyM Identity

runMealy :: Mealy a b -> a -> (b, Mealy a b)
runMealy (MealyM k) x = runIdentity (k x)

-- | @since 0.1.0.0
instance (Applicative m, Semigroup b) => Semigroup (MealyM m a b) where
  MealyM f <> MealyM g = MealyM \x -> liftA2 (<>) (f x) (g x)
  {-# INLINE (<>) #-}

-- | @since 0.1.0.0
instance (Applicative m, Monoid b) => Monoid (MealyM m a b) where
  mempty = MealyM \_ -> pure mempty
  {-# INLINE mempty #-}

-- | @since 0.1.0.0
instance Applicative m => Applicative (MealyM m a) where
  pure x = MealyM \_ -> pure (x, pure x)
  {-# INLINE pure #-}

  MealyM m <*> MealyM n = MealyM \x ->
    liftA2 (\(f, m') (c, n') -> (f c, m' <*> n')) (m x) (n x)
  {-# INLINE (<*>) #-}
