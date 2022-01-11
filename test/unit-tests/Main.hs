module Main (main) where

import Test.Tasty (TestTree, defaultMain, testGroup)

import qualified Test.Control.Comonad.Tape
import qualified Test.Language.Spectacle.Interaction

-- ---------------------------------------------------------------------------------------------------------------------

main :: IO ()
main = defaultMain unitTestTree

unitTestTree :: TestTree
unitTestTree =
  testGroup
    "Unit Tests"
    [ Test.Control.Comonad.Tape.tests
    , Test.Language.Spectacle.Interaction.tests
    ]
