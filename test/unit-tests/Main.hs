module Main (main) where

import Test.Tasty (defaultMain, testGroup)

import qualified Fingerprint

main :: IO ()
main =
  defaultMain $
    testGroup
      "unit tests"
      [ Fingerprint.tests
      ]
