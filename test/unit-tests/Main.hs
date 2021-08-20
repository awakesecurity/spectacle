module Main (main) where

import Test.Tasty (defaultMain, testGroup)

import qualified Spectacle.Checker
import qualified Spectacle.Syntax.Modal

main :: IO ()
main =
  defaultMain $
    testGroup
      "unit tests"
      [ Spectacle.Checker.tests
      , Spectacle.Syntax.Modal.tests
      ]
