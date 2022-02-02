-- |
--
-- @since 0.1.0.0
module Test.Control.Comonad.Tape
  ( tests,
  )
where

import Control.Comonad (duplicate, extract)
import Data.Sequence (Seq)

import Hedgehog (MonadGen, Property, diff, discard, footnote, forAll, property, withTests)
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.Hedgehog (testProperty)

import Control.Comonad.Tape (Tape, shiftl, shiftr, toSeq, viewAt, viewl, viewr)

-- ---------------------------------------------------------------------------------------------------------------------

tests :: TestTree
tests =
  testGroup
    "Tape"
    [ testProperty "Tape.viewl == Tape.viewAt 0" viewlIsAt0
    , testProperty "Tape.viewr == Tape.viewAt . length" viewrIsAtN
    , testProperty "Tape/Seq isomorphism" isoTapeSeq
    , testProperty "extract . duplicate == id" idDuplicateExtract
    , testProperty "move n . viewAt i == viewAt (n + i)" idViewMove
    , testProperty "shiftl . mover . shiftl == shiftl" invShiftl
    , testProperty "mover . shiftl . mover == shiftr" invShiftr
    ]

linearIntSeq :: MonadGen m => Int -> Int -> m (Seq Int)
linearIntSeq lb ub = Gen.seq (Range.linear lb ub) (Gen.int Range.linearBounded)

viewlIsAt0 :: Property
viewlIsAt0 = withTests 10 $ property do
  xs <- forAll (linearIntSeq 0 5)
  diff (viewl xs) (==) (viewAt 0 xs)

viewrIsAtN :: Property
viewrIsAtN = withTests 10 $ property do
  xs <- forAll (linearIntSeq 0 5)
  diff (viewr xs) (==) (viewAt (length xs - 1) xs)

isoTapeSeq :: Property
isoTapeSeq = withTests 10 $ property do
  xs <- forAll (linearIntSeq 0 5)
  i <- forAll (Gen.int $ Range.constant 0 (length xs))

  if null xs
    then diff (toSeq <$> viewAt i xs) (==) Nothing
    else diff (toSeq <$> viewAt i xs) (==) (Just xs)

idDuplicateExtract :: Property
idDuplicateExtract = withTests 10 $ property do
  xs <- forAll (linearIntSeq 0 5)
  i <- forAll (Gen.int $ Range.constant 0 (length xs - 1))

  case viewAt i xs of
    Nothing -> discard
    Just tape -> do
      diff (extract (duplicate tape)) (==) tape

-- Testing that viewing the tape at some @i@ and moving it @n@ is the same as viewing it at @i - n@.
idViewMove :: Property
idViewMove = property do
  xs <- forAll (linearIntSeq 0 10)

  let ub = length xs - 1
  i <- forAll (Gen.int $ Range.constant 0 ub)
  n <- forAll (Gen.int $ Range.constant 0 ub)

  footnote ("viewed at: " ++ show i)
  footnote ("moved by: " ++ show n)

  diff (shiftl n <$> viewAt i xs) (==) (viewAt (i - n) xs)
  diff (shiftr n <$> viewAt i xs) (==) (viewAt (i + n) xs)

invShiftl :: Property
invShiftl = isInverse shiftl shiftr

invShiftr :: Property
invShiftr = isInverse shiftr shiftl

isInverse :: (forall a. Int -> Tape a -> Tape a) -> (forall a. Int -> Tape a -> Tape a) -> Property
isInverse to from = property do
  xs <- forAll (linearIntSeq 0 10)
  let ub = length xs - 1
  i <- forAll (Gen.int $ Range.constant 0 ub)

  case viewAt i xs of
    Nothing -> discard
    Just tape -> do
      n <- forAll (Gen.int $ Range.constant 0 ub)
      diff (to n . from n . to n $ tape) (==) (to n tape)
