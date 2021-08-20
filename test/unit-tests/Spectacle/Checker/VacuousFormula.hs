-- | Unit tests on vacuous temporal formula.
--
-- @since 0.1.0.0
module Spectacle.Checker.VacuousFormula
  ( tests,
  )
where

import Hedgehog (Property, failure, property, success, withTests)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.Hedgehog (testProperty)

import Language.Spectacle (Action, Initial, Invariant, always, modelCheck, unfair)
import Language.Spectacle.Checker.Metrics (ModelMetrics)
import Language.Spectacle.Checker.Model.MCError (MCError)
import Language.Spectacle.Specification (Specification (Specification))

-- ---------------------------------------------------------------------------------------------------------------------

type VacuousSpec = '[]

specInit :: Initial VacuousSpec ()
specInit = return ()

specNext :: Action VacuousSpec Bool
specNext = return True

-- | Test case ensuring that the formula @return False@ fails at initial world generation, in reference to the issue:
-- <https://github.com/awakesecurity/spectacle/issues/26>
testAlwaysVacuousProp :: Property
testAlwaysVacuousProp = withTests 1 . property $ do
  case alwaysCheckSpec of
    Left _ -> success
    Right _ -> failure
  where
    alwaysSpecProp :: Invariant VacuousSpec Bool
    alwaysSpecProp = always (return False)

    alwaysCheckSpec :: Either [MCError VacuousSpec] ModelMetrics
    alwaysCheckSpec = modelCheck (Specification specInit specNext alwaysSpecProp Nothing unfair)

-- | Test case ensuring that the formula @always (return False)@ fails at initial world generation, in reference to the
-- issue: <https://github.com/awakesecurity/spectacle/issues/26>
testVacuousProp :: Property
testVacuousProp = withTests 1 . property $ do
  case checkSpec of
    Left _ -> success
    Right _ -> failure
  where
    specProp :: Invariant VacuousSpec Bool
    specProp = always (return False)

    checkSpec :: Either [MCError VacuousSpec] ModelMetrics
    checkSpec = modelCheck (Specification specInit specNext specProp Nothing unfair)

tests :: TestTree
tests =
  testGroup
    "Spectacle.Checker.VacuousFormula"
    [ testProperty "vacuous formula (always false) is violated" testAlwaysVacuousProp
    , testProperty "vacuous formula (false) is violated" testVacuousProp
    ]
