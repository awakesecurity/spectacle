-- |
--
-- @since 0.1.0.0
module Test.Language.Spectacle.Interaction.Pos
  ( -- * Pos/Interval tests
    tests,

    -- * Generators
    genPos,
    genInterval
  )
where

import Hedgehog (Gen, Property, MonadGen, diff, forAll, property)
import qualified Hedgehog.Gen as Gen
import Hedgehog.Range (constantBounded)
import Lens.Micro (Lens', set)
import Lens.Micro.Extras (view)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.Hedgehog (testProperty)

import Language.Spectacle.Interaction.Pos (Pos (Pos), Interval (Interval), pcol, prow)

-- ---------------------------------------------------------------------------------------------------------------------

tests :: TestTree
tests =
  testGroup
    "Language.Spectacle.Interaction.Pos"
    [ testsPos
    ]

-- ---------------------------------------------------------------------------------------------------------------------

testsPos :: TestTree
testsPos =
  testGroup
    "Language.Spectacle.Interaction.Pos.Pos"
    [ testProperty "∀ (x y u v :: Int) -> x < u -> Pos x y < Pos u bv" propRowOrdering
    , testProperty "prow lens coherence" propRowLens
    , testProperty "pcol lens coherence" propColLens
    ]

propRowOrdering :: Property
propRowOrdering = property do
  -- Test that guarantees the order of columns is irrelevant if rows are unequal.
  x <- forAll (Gen.int constantBounded)
  y <- forAll (Gen.int constantBounded)
  if x < y
    then diff (Pos x y) (<) (Pos y x)
    else diff (Pos y x) (<) (Pos x y)

propRowLens :: Property
propRowLens = do
  -- The row lens behaves
  propLensLaws prow Pos (Gen.int constantBounded)

propColLens :: Property
propColLens = do
  -- The column lens behaves
  propLensLaws pcol Pos (Gen.int constantBounded)

genPos :: MonadGen m => m Pos
genPos = Pos <$> Gen.int constantBounded <*> Gen.int constantBounded

genInterval :: MonadGen m => m Interval
genInterval = Interval <$> Gen.int constantBounded <*> Gen.int constantBounded

-- ---------------------------------------------------------------------------------------------------------------------

propLensLaws :: (Eq a, Show a, Eq b, Show b) => Lens' a b -> (b -> b -> a) -> Gen b -> Property
propLensLaws p c gen = property do
  s <- c <$> forAll gen <*> forAll gen
  x <- forAll gen
  y <- forAll gen
  diff (view p $ set p x s) (==) x
  diff (set p (view p s) s) (==) s
  diff (set p y $ set p x s) (==) (set p y s)
