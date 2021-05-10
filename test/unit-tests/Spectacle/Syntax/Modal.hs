module Spectacle.Syntax.Modal
  ( tests,
  )
where

import Test.Tasty (TestTree, testGroup)

import qualified Spectacle.Syntax.Modal.Properties

-- -------------------------------------------------------------------------------------------------

tests :: TestTree
tests =
  testGroup
    "Spectacle.Syntax.Modal"
    [ Spectacle.Syntax.Modal.Properties.tests
    ]
