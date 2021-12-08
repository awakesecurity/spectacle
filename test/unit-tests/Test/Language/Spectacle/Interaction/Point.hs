-- |
--
-- @since 0.1.0.0
module Test.Language.Spectacle.Interaction.Point
  ( -- * Point Tests
    tests,

    -- * Generators
    genPoint,
    genNilWorld,
  )
where

import Hedgehog (Gen, MonadGen, Property, diff, forAll, property)
import qualified Hedgehog.Gen as Gen
import Hedgehog.Range (constantBounded)
import Lens.Micro (Lens', set)
import Lens.Micro.Extras (view)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.Hedgehog (testProperty)

import Data.Context
import Data.Type.Rec
import Data.World
import Language.Spectacle.Interaction.Point
import Language.Spectacle.Interaction.Pos

import Test.Language.Spectacle.Interaction.Pos (genPos, genInterval)

-- ---------------------------------------------------------------------------------------------------------------------

tests :: TestTree
tests =
  testGroup
    "Language.Spectacle.Interaction.Point"
    [ testsPoint
    ]

-- ---------------------------------------------------------------------------------------------------------------------

testsPoint :: TestTree
testsPoint =
  testGroup
    "Language.Spectacle.Interaction.Point.Point"
    [
    ]

genPoint :: MonadGen m => m (Point 'CtxtNil)
genPoint =
  Point
    <$> genNilWorld
    <*> pure Nothing
    <*> genInterval
    <*> genPos

genNilWorld :: MonadGen m => m (World 'CtxtNil)
genNilWorld = do
  hash <- Gen.int constantBounded
  pure (World (fromIntegral hash) RNil)
