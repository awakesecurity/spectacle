module Checker
  ( tests,
  )
where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.Hedgehog (testProperty)

import qualified Checker.Diehard as Diehard

-- -----------------------------------------------------------------------------

tests :: TestTree
tests =
  testGroup
    "Language.Spectacle.Checker"
    [ testProperty "model checking diehard" $ Diehard.prop
    ]
