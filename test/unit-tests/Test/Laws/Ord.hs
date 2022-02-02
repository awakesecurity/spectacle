-- | Laws for total orders.
--
-- @since 0.1.0.0
module Test.Laws.Ord
  ( -- * Ord Laws
    laws,
    irreflexive,
    antisymmetry,
  )
where

import Hedgehog (Gen, PropertyT, assert, failure, footnote, forAll, property, success, withTests)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.Hedgehog (testProperty)

-- ---------------------------------------------------------------------------------------------------------------------

laws :: (Show a, Ord a) => String -> Gen a -> TestTree
laws desc gen =
  testGroup
    (if null desc then "ord laws" else desc ++ " - ord laws")
    [ testProperty "1. antisymmetry" $ property (antisymmetry gen)
    , testProperty "2. irreflexive" . withTests 1 $ property (irreflexive gen)
    ]

-- | Strict order is irreflexive
--
-- @
-- a < b <=> a /= b
-- @
irreflexive :: (Monad m, Show a, Ord a) => Gen a -> PropertyT m ()
irreflexive gen = do
  a <- forAll gen
  footnote (show a ++ " < " ++ show a)
  if a < a
    then failure
    else success

-- | Strict order is antisymmetric
--
-- @
-- a < b <=> not (b < a)
-- @
antisymmetry :: (Monad m, Show a, Ord a) => Gen a -> PropertyT m ()
antisymmetry gen = do
  a <- forAll gen
  b <- forAll gen
  footnote (show a ++ " < " ++ show b)
  if a < b
    then assert (b >= a)
    else assert (a >= b)
