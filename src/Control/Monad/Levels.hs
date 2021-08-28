-- @since 0.1.0.0
module Control.Monad.Levels
  ( -- * LevelsT
    LevelsT (LevelsT, runLevelsT),
    runLevelsA,
    liftLevelsT,
    wrapLevelsT,

    -- ** Searches
    star,
    starInt,
    choices,
    choicesInt,
    foreach,
  )
where

import Control.Applicative (liftA2, Alternative (empty, (<|>)))
import Data.Kind (Type)

import Data.Bag (Bag (None))
import qualified Data.Bag as Bag
import Control.Monad.Levels.Internal

-- ---------------------------------------------------------------------------------------------------------------------


runLevelsA :: Alternative m => LevelsT m a -> m (Bag a)
runLevelsA (LevelsT m) = m ((<|>) . pure) (pure None)
{-# INLINE runLevelsA #-}

star :: (Alternative m, Monad m) => (a -> m a) -> a -> m a
star f x = pure x <|> (f x >>= star f)
{-# INLINE star #-}

starInt :: (Alternative m, Monad m) => (Int -> a -> m a) -> Int -> a -> m a
starInt f n x = pure x <|> (f n x >>= starInt f (n + 1))
{-# INLINE starInt #-}

choices :: (Alternative f, Foldable t) => (a -> f b) -> t a -> f b
choices f = foldr ((<|>) . f) empty
{-# INLINE choices #-}

foreach :: (Alternative f, Monoid (m b), Foldable t) => t a -> (a -> f (m b)) -> f (m b)
foreach xs f = foldr (liftA2 (<>) . f) (pure mempty) xs
{-# INLINE foreach #-}

choicesInt :: (Alternative f, Foldable t) => (Int -> a -> f b) -> Int -> t a -> f b
choicesInt f n = foldr ((<|>) . f (n + 1)) empty
{-# INLINE choicesInt #-}
