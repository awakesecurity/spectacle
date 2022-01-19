module Test.Language.Spectacle.Interaction
  ( tests,
  )
where

import Test.Tasty (TestTree, testGroup)

import qualified Test.Language.Spectacle.Interaction.Paths
import qualified Test.Language.Spectacle.Interaction.Pos

-- ---------------------------------------------------------------------------------------------------------------------

tests :: TestTree
tests =
  testGroup
    "Interaction"
    [ Test.Language.Spectacle.Interaction.Paths.tests
    , Test.Language.Spectacle.Interaction.Pos.tests
    ]
