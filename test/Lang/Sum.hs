{-# LANGUAGE AllowAmbiguousTypes #-}

module Lang.Sum
  ( tests,
  )
where

import Control.Monad.Trans.Identity (IdentityT)
import Control.Monad.Trans.Maybe (MaybeT)
import Data.Functor.Identity (Identity)
import Hedgehog (Gen, Property, failure, forAll, property, (===))
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.Hedgehog (testProperty)

import Language.Spectacle.Lang.Sum
  ( Sum,
    decompose,
    extract,
    inject,
    project,
    weaken,
  )

-- -----------------------------------------------------------------------------

sumUnion :: IdentityT Identity a -> Sum '[IdentityT, IdentityT] a
sumUnion = weaken . inject

sumDisjoint :: IdentityT Identity a -> Sum '[MaybeT, IdentityT] a
sumDisjoint = weaken . inject

-- | weakening preserves project . inject = id.
prop_weaken_idem_disjoint :: (Show a, Eq a) => Gen a -> Property
prop_weaken_idem_disjoint genA = property do
  x <- pure <$> forAll genA
  project (sumDisjoint x) === Just x

-- | Weakening preserves project . inject = id under composition even if the
-- target of the weakening already exists in a sum's type universe.
--
-- This is an important case to consider because it /was/ possible for the inner
-- 'Word' used to index the type universe to get confused in these scenarios.
prop_weaken_idem_union :: (Show a, Eq a) => Gen a -> Property
prop_weaken_idem_union genA = property do
  x <- pure <$> forAll genA
  project (sumUnion x) === Just x

-- | `decompose . weaken . inject = id` where the target of weakening already
-- exists in the sum's universe is another important case for the same reasons
-- as 'prop_weaken_idem_union'.
prop_decompose_idem_union :: (Show a, Eq a) => Gen a -> Property
prop_decompose_idem_union genA = property do
  x <- pure <$> forAll genA
  case decompose (sumUnion x) of
    Left _ -> failure
    Right x' -> x' === x

-- | Probably not necessary but provided for completeness.
prop_extract :: forall a. (Show a, Eq a) => Gen a -> Property
prop_extract genA = property do
  x :: IdentityT Identity a <- pure <$> forAll genA
  let aSum :: Sum '[IdentityT] a
      aSum = inject x
  extract aSum === x

tests :: TestTree
tests =
  testGroup
    "Data.Extensible.Sum"
    [ testGroup
        "project . weaken . inject = id"
        [ testProperty "unioned universe" $ prop_weaken_idem_union gen
        , testProperty "disjoint universe" $ prop_weaken_idem_disjoint gen
        ]
    , testProperty "decompose . weaken . inject === id" $ prop_decompose_idem_union gen
    , testProperty "extract . inject = id" $ prop_extract gen
    ]
  where
    gen :: Gen Int
    gen = Gen.int (Range.constant 0 1000)
