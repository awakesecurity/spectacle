-- |
--
-- @since 0.1.0.0
module Test.Language.Spectacle.Interaction.Paths
  ( tests,
  )
where

import Hedgehog (Gen, MonadGen, Property, Range, annotate, assert, annotateShow, diff, forAll, property)
import qualified Hedgehog.Gen as Gen
import Hedgehog.Range (constantBounded, constant)
import Lens.Micro (Lens', set, (^.))
import Lens.Micro.Extras (view)
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Foldable (length)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.Hedgehog (testProperty)

import Data.Context (CNil)
import Language.Spectacle.Interaction.Paths (Paths(Paths), getPaths)
import qualified Language.Spectacle.Interaction.Paths as Paths
import Language.Spectacle.Interaction.Point
import Language.Spectacle.Interaction.Pos

import Test.Language.Spectacle.Interaction.Point (genPoint)

-- ---------------------------------------------------------------------------------------------------------------------

tests :: TestTree
tests =
  testGroup
    "Language.Spectacle.Interaction.Paths"
    [ -- Destruction
      testProperty "Paths.sliceRow" propSliceRow

      -- Query
    , testProperty "Paths.isLabel" propIsLabel
    , testProperty "Paths.isPoint" propIsPoint
    , testProperty "Paths.isPointEmpty" propIsPointEmpty
    ]

propSliceRow :: Property
propSliceRow = property do
  -- Tests if 'sliceRow' is morally equivalent to a special case of 'Set.filter' which is what we expect.
  rowIx <- forAll (Gen.int constantBounded)
  paths <- forAll (genPaths $ constant 0 4)

  let row = Set.filter (\pt -> rowIx == view (ptpos . prow) pt) (getPaths paths)

  diff (Paths.sliceRow rowIx paths) (==) row

propIsLabel :: Property
propIsLabel = property do
  -- Tests querying the membership of a fingerprint @hash@ in a generated 'Paths' with 'isLabel'.
  set <- forAll (Gen.set (constant 0 4) genPoint)
  let paths = Paths set
  if length set <= 0
    then do
      hash <- fromIntegral <$> forAll (Gen.int constantBounded)
      annotateShow paths
      diff hash Paths.isNotLabel paths
    else do
      idx <- forAll (Gen.int $ constant 0 (length set - 1))
      let hash = Set.elemAt idx set ^. ptlabel
      diff hash Paths.isLabel paths

propIsPoint :: Property
propIsPoint = property do
  -- Tests membership of 'Pos' for arbitrarily sized 'Paths', not many elements need to be inserted.
  pt <- forAll genPoint
  paths <- forAll (genPaths $ constant 0 4)
  diff (view ptpos pt) Paths.isPoint (Paths.insert pt paths)

propIsPointEmpty :: Property
propIsPointEmpty = property do
  pt <- forAll genPoint
  let paths = mempty :: Paths CNil
  diff (view ptpos pt) Paths.isNotPoint paths

genPaths :: MonadGen m => Range Int -> m (Paths CNil)
genPaths rng = Paths <$> Gen.set rng genPoint
