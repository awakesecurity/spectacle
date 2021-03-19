module Spec
  ( tests,
  )
where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.Hedgehog (testProperty)

-- https://github.com/awakesecurity-dev/spectacle/issues/15
-- import qualified Spec.CyclicDependencies as CyclicDependencies
import qualified Spec.Exceptions as Exceptions
import qualified Spec.Sharing as Sharing

-- -----------------------------------------------------------------------------

tests :: TestTree
tests =
  testGroup
    "Language.Spectacle.SpecVM"
    [ testGroup
        "observable sharing"
        [ testProperty "from right to left" $ Sharing.prop_shares_right
        , testProperty "from left to right" $ Sharing.prop_shares_left
        ]
    , testProperty "catches exceptions" $ Exceptions.prop_exceptions
    --, testProperty "cyclic dependencies" $ CyclicDependencies.prop_cyclic
    ]
