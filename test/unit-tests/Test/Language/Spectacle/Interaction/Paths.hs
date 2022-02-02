module Test.Language.Spectacle.Interaction.Paths
  ( tests,
  )
where

import Data.Foldable (find, for_)
import Lens.Micro.Extras (view)

import Hedgehog (Property, evalMaybe, forAll, property)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.Hedgehog (testProperty)

import Data.World (World (World))
import Language.Spectacle.Interaction.Paths as Paths (toPointSet)
import Language.Spectacle.Interaction.Point (label)

import qualified Test.Gen as Gen

-- ---------------------------------------------------------------------------------------------------------------------

tests :: TestTree
tests =
  testGroup
    "Paths"
    [ testProperty "Paths.toPointSet is nondestructive" flattenNondestruct
    ]

-- | Paths.flatten does not destroy or add information.
flattenNondestruct :: Property
flattenNondestruct = property do
  -- tree size is adjusted by hedgehog's size parameter.
  tree <- forAll (Gen.tree Gen.emptyWorld)
  let set = Paths.toPointSet tree

  for_ tree \(World hash _) ->
    evalMaybe $ find (look hash) set
  where
    -- ensure membership for each element of the original tree and the flattened as 'Paths' by it's hash.
    look hash1 world = hash1 == view label world
