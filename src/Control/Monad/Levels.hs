-- |
--
-- @since 0.1.0.0
module Control.Monad.Levels
  ( -- * Levels
    Levels,
    runLevels,

    -- * LevelsT
    LevelsT (LevelsT),
    runLevelsT,
    observeLevelsT,
    execLevelsT,
    runLevelsA,
    wrap,
    liftLevelsT,
    wrapLevelsT,
    foldMapAp,
    forAp,
    foldMapAlt,
    wrap,
  )
where

import Control.Applicative (Alternative, empty, liftA2, (<|>))
import Data.Foldable (Foldable, fold)

import Control.Monad.Levels.Internal (Levels, LevelsT (LevelsT), liftLevelsT, runLevels, runLevelsT, wrapLevelsT)
import Data.Bag (Bag (None))
import qualified Data.Bag as Bag

-- ---------------------------------------------------------------------------------------------------------------------

observeLevelsT :: Applicative m => LevelsT m a -> m [a]
observeLevelsT (LevelsT m) = m (fmap . (++) . foldMap pure) (pure [])
{-# INLINE observeLevelsT #-}

execLevelsT :: Applicative m => LevelsT m a -> m ()
execLevelsT (LevelsT m) = m (const id) (pure ())
{-# INLINE execLevelsT #-}

runLevelsA :: Alternative m => LevelsT m a -> m (Bag a)
runLevelsA (LevelsT m) = m ((<|>) . pure) (pure None)

foldMapAp :: (Foldable t, Monoid m, Applicative f) => (a -> f m) -> t a -> f m
foldMapAp f = foldr (liftA2 (<>) . f) (pure mempty)

forAp :: (Foldable t, Monoid m, Applicative f) => t a -> (a -> f m) -> f m
forAp = flip foldMapAp

foldMapAlt :: (Foldable t, Alternative m) => (a -> m b) -> t a -> m b
foldMapAlt f = foldr ((<|>) . f) empty

wrap :: LevelsT m a -> LevelsT m a
wrap (LevelsT k) = LevelsT (\cons nil -> cons Bag.empty (k cons nil))
