module Main (main) where

import Test.Tasty (defaultMain, testGroup)

import qualified Spectacle.Checker.VacuousFormula
import qualified Spectacle.Syntax.Modal

main :: IO ()
main =
  defaultMain $
    testGroup
      "unit tests"
      [ Spectacle.Syntax.Modal.tests
      , Spectacle.Checker.VacuousFormula.tests
      ]
