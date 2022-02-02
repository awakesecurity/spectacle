module Test.Gen
  ( -- * Tree Generators
    tree,
    cataTree,
    subtrees,
    leaves,

    -- * Fingerprint
    fingerprint,

    -- * World
    emptyWorld,

    -- * Pos
    pos,

    -- * Re-exports
    Fingerprint,
    Pos,
    Tree,
    World,
    module Hedgehog.Gen,
  )
where

import Control.Applicative (liftA2)
import Control.Monad (replicateM)

import Hedgehog (MonadGen)
import Hedgehog.Gen (choice, int, resize, sized)
import Hedgehog.Internal.Gen (golden)
import Hedgehog.Range (Size, constantBounded, linear, linearBounded)

import Data.Fingerprint (Fingerprint (Fingerprint))
import Data.Functor.Tree (Tree (Node), pattern Leaf)
import Data.World (World (World))
import Language.Spectacle.Interaction.Pos (Pos, pattern Pos)

import qualified Test.Gen.Rec as Gen.Rec

-- ---------------------------------------------------------------------------------------------------------------------

-- | @'tree' x@ recursive generates a tree of @x@ using both 'subtrees' and 'leaves'.
tree :: MonadGen m => m a -> m (Tree a)
tree gen = sized (liftA2 Node gen . fmap pure . generator)
  where
    generator !size
      | size <= 1 = Leaf <$> gen
      | otherwise = do
        len <- int (linear 0 $ fromIntegral size)
        liftA2 Node gen $ choice [subtrees len gen, leaves len gen]

-- | @'cataTree' x k@ is a recursive 'Tree' generator for an element generator @x@, subtree generator @k@. The subtree
-- generator passes a suggested a subtree length (scaled by the size parameter) along with the generator for @x@.
--
-- 'cataTree' replaces 'tree' in instances where a custom generator (such as 'subtrees' or 'leaves') is needed.
cataTree :: MonadGen m => m a -> (Int -> m a -> m [Tree a]) -> m (Tree a)
cataTree gen k = sized (liftA2 Node gen . fmap pure . generator)
  where
    generator !size
      | size <= 1 = Leaf <$> gen
      | otherwise = do
        len <- int (linear 0 $ fromIntegral size)
        liftA2 Node gen $ k len gen

-- | @'subtrees' n x@ generates @n@ subtrees of @x@ dividing the size parameter among them evenly.
subtrees :: MonadGen m => Int -> m a -> m [Tree a]
subtrees len gen =
  sized \size ->
    if 1 < size
      then resize (smallN len size) do
        replicateM len (cataTree gen subtrees)
      else pure []

-- | @'leaves' n x@ is constant to n-many leaves of @x@.
leaves :: MonadGen m => Int -> m a -> m [Tree a]
leaves len gen = replicateM len (fmap Leaf gen)

-- ---------------------------------------------------------------------------------------------------------------------

fingerprint :: MonadGen m => m Fingerprint
fingerprint = Fingerprint <$> int constantBounded

-- ---------------------------------------------------------------------------------------------------------------------

emptyWorld :: MonadGen m => m (World '[])
emptyWorld = liftA2 World fingerprint Gen.Rec.empty

-- ---------------------------------------------------------------------------------------------------------------------

pos :: MonadGen m => m Pos
pos = liftA2 Pos (int linearBounded) (int linearBounded)

-- ---------------------------------------------------------------------------------------------------------------------

-- | Like 'small', but scales the size parameter by a factor of @n@.
smallN :: Int -> Size -> Size
smallN n size
  | 1 < n = golden (size `quot` fromIntegral n)
  | otherwise = 0
