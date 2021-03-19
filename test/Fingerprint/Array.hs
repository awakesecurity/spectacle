module Fingerprint.Array
  ( tests,
  )
where

import Control.Monad (foldM)
import GHC.Exts (fromList, toList)
import Hedgehog
  ( MonadGen,
    Property,
    forAll,
    property,
    (===),
  )
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.Hedgehog (testProperty)

import Data.Fingerprint.Array (Array, insertAt, updateAt)

-- -----------------------------------------------------------------------------

-- | Shorthand list generator.
genList :: MonadGen m => m [Int]
genList = Gen.list (Range.constant 0 100) (Gen.int (Range.constant 0 1000))

-- | Generate a tuple containing a index and a random value respectively.
genIx :: MonadGen m => Int -> m (Int, Int)
genIx maxIx = do
  ix <- Gen.int (Range.constant 0 maxIx)
  x <- Gen.int (Range.constant 0 1000)
  return (ix, x)

-- | Generates a indexed-list. It generates an initial random size for the list
-- then keeps track of that while generating random indicies bounded by the
-- current list size.
--
-- This is so we never generate indexed out of bounds for example:
--
-- @
-- [(0, 61), (0, 123), (2, 98), (4, 9211), ... (ix, a)]
--   |        |         |        |
--   0 <= 1   0 <= 1    2 <= 3   4 <= 4    ...  ix <= n
-- @
genIList :: MonadGen m => m [(Int, Int)]
genIList = Gen.int (Range.constant 0 100) >>= go 0
  where
    go :: MonadGen m => Int -> Int -> m [(Int, Int)]
    go _ 0 = return []
    go ix n = (:) <$> genIx ix <*> go (ix + 1) (n - 1)

-- | toList (fromList xs) === xs
prop_isListIso :: Property
prop_isListIso = property $ do
  xs <- forAll genList
  let arr :: Array Int
      arr = fromList xs
  toList arr === xs

-- | Testing that insertAt is equivalent to:
--
-- @
-- take n xs <> [x] <> drop n xs
-- @
prop_insertAt :: Property
prop_insertAt = property $ do
  ixs <- forAll genIList
  fps <- foldM (\xs (n, x) -> return (insertAt xs n x)) mempty ixs
  xs <- foldM (\xs (n, x) -> return (insertAtList xs n x)) mempty ixs
  toList fps === xs
  where
    insertAtList :: [Int] -> Int -> Int -> [Int]
    insertAtList xs n x = take n xs <> [x] <> drop n xs

-- | Testing updateAt is equal to it's list counterpart.
prop_updateAt :: Property
prop_updateAt = property $ do
  ys <- forAll genList
  n <- forAll $ Gen.int (Range.constant 0 (length ys))
  x <- forAll $ Gen.int (Range.constant 0 10000)
  let xs :: Array Int
      xs = fromList ys
  toList (updateAt xs n x) === updateAtList n x ys
  where
    updateAtList :: Int -> a -> [a] -> [a]
    updateAtList _ _ [] = []
    updateAtList n x (y : xs)
      | n == 0 = x : xs
      | otherwise = y : updateAtList (n - 1) x xs

tests :: TestTree
tests =
  testGroup
    "Data.Fingerprint.Array"
    [ testProperty "fromList . toList = id" prop_isListIso
    , testProperty "insertAt" prop_insertAt
    , testProperty "updateAt" prop_updateAt
    ]
