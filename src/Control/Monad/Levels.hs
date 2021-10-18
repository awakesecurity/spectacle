-- @since 0.1.0.0
module Control.Monad.Levels
  ( -- * LevelsT
    LevelsT (LevelsT, runLevelsT),
    runLevelsA,
    liftLevelsT,
    wrapLevelsT,
    zipLevelsWithT,
    foldMapAp,
    forAp,
    foldMapAlt,
    forAlt,
    (<>=),
  )
where

import Control.Applicative (Alternative (empty, (<|>)), liftA2)

import Control.Monad.Levels.Internal (LevelsT (LevelsT), liftLevelsT, runLevelsT, wrapLevelsT, zipLevelsWithT)
import Data.Bag (Bag (None))

-- ---------------------------------------------------------------------------------------------------------------------

runLevelsA :: Alternative m => LevelsT m a -> m (Bag a)
runLevelsA (LevelsT m) = m ((<|>) . pure) (pure None)
{-# INLINE runLevelsA #-}

foldMapAp :: (Foldable t, Monoid m, Applicative f) => (a -> f m) -> t a -> f m
foldMapAp f = foldr (liftA2 (<>) . f) (pure mempty)
{-# INLINE foldMapAp #-}

forAp :: (Foldable t, Monoid m, Applicative f) => t a -> (a -> f m) -> f m
forAp = flip foldMapAp
{-# INLINE forAp #-}

foldMapAlt :: (Foldable t, Alternative m) => (a -> m b) -> t a -> m b
foldMapAlt f = foldr ((<|>) . f) empty
{-# INLINE foldMapAlt #-}

forAlt :: (Foldable t, Alternative f) => t a -> (a -> f b) -> f b
forAlt = flip foldMapAlt
{-# INLINE forAlt #-}

infixl 1 <>=
(<>=) :: (Monad m, Foldable f, Monoid (t b)) => m (f a) -> (a -> m (t b)) -> m (t b)
xs <>= k = xs >>= foldr (liftA2 (<>) . k) (pure mempty)
{-# INLINE (<>=) #-}
