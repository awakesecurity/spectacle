-- |
--
-- @since 0.1.0.0
module Functor.Temporal.Basic where

import Data.Functor.Identity
import Hedgehog (Property, assert, forAll, property, withTests, (/==), (===))
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.Hedgehog (testProperty)

import Data.Functor.Temporal.Basic

-- ---------------------------------------------------------------------------------------------------------------------

prop_respIdentityK :: Property
prop_respIdentityK = withTests 1 . property $ do
  let k :: K Identity Identity Int Int = pure 0
      f :: Identity (Int -> Identity Int) = Identity (pure . (+ 5))

  runK (fmap id k) f === fmap id (runK k f)

prop_respCompK :: Property
prop_respCompK = withTests 1 . property $ do
  let k :: K Identity Identity Int Int = pure 0
      f :: Int -> Int = (+ 5)
      g :: Int -> Int = \x -> x - 5
      h :: Identity (Int -> Identity Int) = Identity (pure . (+ 5))

  runK (fmap (g . f) k) h === runK (fmap g (fmap f k)) h

tests :: TestTree
tests =
  testGroup
    "Functor.Temporal.Basic"
    [ testProperty "respects identity" prop_respIdentityK
    , testProperty "respects composition" prop_respCompK
    ]
