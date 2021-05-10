
module Spectacle.Syntax.Logic
  ( tests,
  )
where

import Control.Monad
import Data.Function ((&))
import Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import Test.Tasty
import Test.Tasty.Hedgehog

import Language.Spectacle.Lang
import Language.Spectacle.Syntax.Error
import Language.Spectacle.Syntax.NonDet
import Language.Spectacle.Syntax.Logic
import Language.Spectacle.Exception.RuntimeException

-- -------------------------------------------------------------------------------------------------

prop_negationInvolute :: Property
prop_negationInvolute = property do
  let negated = runLogical (complement (complement (pure True)))
  case negated of
    Left exc -> do
      footnoteShow exc
      failure
    Right x -> x === [True]

evensForall ::
  Members '[Logic, NonDet, Error RuntimeException] effs =>
  [Int] ->
  Lang ctx effs Int
evensForall xs = forall xs (pure . even)

oddsExists ::
  Members '[Logic, NonDet, Error RuntimeException] effs =>
  [Int] ->
  Lang ctx effs Int
oddsExists xs = exists xs (pure . odd)

runLogical ::
  Lang ctx '[Logic, NonDet, Error RuntimeException] a ->
  Either RuntimeException [a]
runLogical = runLang . runError . runNonDetA . runLogic

prop_forallNegated :: Property
prop_forallNegated = property do
  let xs = [1 :: Int .. 5]
  ex <- case runLogical (oddsExists xs) of
    Left exc -> do
      footnoteShow exc
      failure
    Right xs' -> pure xs'
  fa <- case runLogical (complement (evensForall xs)) of
    Left exc -> do
      footnoteShow exc
      failure
    Right xs' -> pure xs'
  ex === fa

prop_existsNegated :: Property
prop_existsNegated = property do
  let xs = [2 :: Int, 4, 6, 8]
  ex <- case runLogical (complement (oddsExists xs)) of
    Left exc -> do
      footnoteShow exc
      failure
    Right xs' -> pure xs'
  fa <- case runLogical (evensForall xs) of
    Left exc -> do
      footnoteShow exc
      failure
    Right xs' -> pure xs'
  ex === fa

tests :: TestTree
tests =
  testGroup
    "Spectacle.Syntax.Logic"
    [ testProperty
        "Negation effect involutes over booleans."
        prop_negationInvolute
    , testProperty
        "Negating forall p is exists (not . p)"
        prop_forallNegated
    , testProperty
        "Negating exists p is forall (not . p)"
        prop_existsNegated
    ]
