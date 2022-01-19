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
    runLevelsM,
    wrap,
    foldAlt,
    liftLevelsT,
    wrapLevelsT,
    zipLevelsWithT,
    zipLevelsWith,
  )
where

import Control.Applicative (Alternative ((<|>)))
import Data.Foldable (Foldable, fold)

import Control.Monad.Levels.Internal (LevelsT (LevelsT), liftLevelsT, runLevelsT, wrapLevelsT, zipLevelsWithT)
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
{-# INLINE runLevelsA #-}

runLevelsM :: (Applicative f, Monoid m) => LevelsT f m -> f m
runLevelsM (LevelsT m) = m (fmap . mappend . fold) (pure mempty)

wrap :: LevelsT m a -> LevelsT m a
wrap (LevelsT k) = LevelsT (\cons nil -> cons Bag.empty (k cons nil))

-- (runLevelsT xs (\x xs -> pure (Just (x, Concrete.LevelsT xs))) (pure Nothing))
