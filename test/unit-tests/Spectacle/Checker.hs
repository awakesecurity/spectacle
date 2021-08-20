-- |

module Spectacle.Checker
  ( tests
  )
where

import Test.Tasty (TestTree, testGroup)

import qualified Spectacle.Checker.CyclicLiveness
import qualified Spectacle.Checker.VacuousFormula

-- ---------------------------------------------------------------------------------------------------------------------

tests :: TestTree
tests =
  testGroup
    "Spectacle.Checker"
    [ Spectacle.Checker.CyclicLiveness.tests
    , Spectacle.Checker.VacuousFormula.tests
    ]
