-- |
--
-- @since 1.0.0
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
    liftLevelsT,
    wrapLevelsT,
  )
where

import Control.Applicative (Alternative, (<|>))

import Control.Monad.Levels.Internal (Levels, LevelsT (LevelsT), liftLevelsT, runLevels, runLevelsT, wrapLevelsT)
import Data.Bag (Bag (None))

-- ---------------------------------------------------------------------------------------------------------------------

observeLevelsT :: Applicative m => LevelsT m a -> m [a]
observeLevelsT (LevelsT m) = m (fmap . (++) . foldMap pure) (pure [])

execLevelsT :: Applicative m => LevelsT m a -> m ()
execLevelsT (LevelsT m) = m (const id) (pure ())

runLevelsA :: Alternative m => LevelsT m a -> m (Bag a)
runLevelsA (LevelsT m) = m ((<|>) . pure) (pure None)
