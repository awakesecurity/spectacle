module Test.Language.Spectacle.Interaction.Pos
  ( -- * Pos/Interval tests
    tests,
  )
where

import Control.Monad (when)
import Lens.Micro.Extras (view)

import Hedgehog (Property, diff, forAll, property)
import Hedgehog.Range (linearBounded)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.Hedgehog (testProperty)

import Language.Spectacle.Interaction.Pos (pcol, prow)

import qualified Test.Gen as Gen
import qualified Test.Laws.Lens
import qualified Test.Laws.Ord

-- ---------------------------------------------------------------------------------------------------------------------

tests :: TestTree
tests =
  testGroup
    "Pos"
    [ testsPos
    , testsInterval
    ]

-- ---------------------------------------------------------------------------------------------------------------------

testsPos :: TestTree
testsPos =
  testGroup
    "Pos.Pos"
    [ testProperty "row ordering" propRowOrder
    , Test.Laws.Ord.laws "pos" Gen.pos
    , Test.Laws.Lens.laws pcol "pcol" Gen.pos (Gen.int linearBounded)
    , Test.Laws.Lens.laws prow "prow" Gen.pos (Gen.int linearBounded)
    ]

-- | This property characterizes the order on 'Pos'.
propRowOrder :: Property
propRowOrder = property do
  p <- forAll Gen.pos
  q <- forAll Gen.pos
  when (view prow p < view prow q) do
    diff p (<) q

-- ---------------------------------------------------------------------------------------------------------------------

testsInterval :: TestTree
testsInterval =
  testGroup
    "Pos.Interval"
    [ Test.Laws.Ord.laws "interval" Gen.pos
    ]
