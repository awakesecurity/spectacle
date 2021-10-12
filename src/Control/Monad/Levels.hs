-- @since 0.1.0.0
module Control.Monad.Levels
  ( -- * LevelsT
    LevelsT (LevelsT, runLevelsT),
    runLevelsA,
    runLevelsM,
    sumFoldable,
    liftLevelsT,
    wrapLevelsT,
    zipLevelsWithT,
    zipLevelsWith,
    foldMapAp,
    forAp,
    foldMapAlt,
    forAlt,
    (<>=),
  )
where

import Control.Applicative (Alternative (empty, (<|>)), liftA2)
import Data.Foldable

import Control.Monad.Levels.Internal
  ( LevelsT (LevelsT),
    liftLevelsT,
    runLevelsT,
    wrapLevelsT,
    zipLevelsWith,
    zipLevelsWithT,
  )
import Data.Bag (Bag (None))
import qualified Data.Bag as Bag

-- ---------------------------------------------------------------------------------------------------------------------

runLevelsA :: Alternative m => LevelsT m a -> m (Bag a)
runLevelsA (LevelsT m) = m ((<|>) . pure) (pure None)
{-# INLINE runLevelsA #-}

runLevelsM :: (Applicative f, Monoid m) => LevelsT f m -> f m
runLevelsM (LevelsT m) = m (fmap . mappend . fold) (pure mempty)

-- | Constructs a 'LevelsT' with a single level, the monoid provided.
--
-- @since 0.1.0.0
sumFoldable :: Foldable m => m a -> LevelsT f a
sumFoldable xs = LevelsT \cons nil -> cons (foldr Bag.cons Bag.empty xs) nil
{-# INLINE sumFoldable #-}

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
