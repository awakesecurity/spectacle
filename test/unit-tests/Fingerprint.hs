module Fingerprint
  ( tests,
  )
where

import Hedgehog (MonadGen, Property, annotate, annotateShow, failure, forAll, property, (===))
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.Hedgehog (testProperty)

import Text.Megaparsec (runParser)

import Language.Spectacle.Checker.Fingerprint (Fingerprint(Fingerprint), getFingerprint)
import Language.Spectacle.Interaction.Parse (parseFingerprint)

-- ---------------------------------------------------------------------------------------------------------------------

genFingerprint :: MonadGen m => m Fingerprint
genFingerprint = Fingerprint <$> Gen.word32 Range.constantBounded

prop_fingerPrintNominalParseIso :: Property
prop_fingerPrintNominalParseIso = property do
  fpInt <- forAll genFingerprint

  case runParser parseFingerprint "<no name>" (show fpInt) of
    Left err -> do
      annotate "could not parse fingerprint"
      annotateShow err
      failure
    Right fpStr -> fpInt === fpStr

prop_fingerPrintRepParseIso :: Property
prop_fingerPrintRepParseIso = property do
  fpInt <- forAll genFingerprint

  case runParser parseFingerprint "<no name>" (show fpInt) of
    Left err -> do
      annotate "could not parse fingerprint"
      annotateShow err
      failure
    Right fpStr -> getFingerprint fpInt === getFingerprint fpStr

tests :: TestTree
tests =
  testGroup
    "Data.Fingerprint"
    [ testProperty "id = parse . show . fingerprint" $ prop_fingerPrintNominalParseIso
    , testProperty "id = parse . show . fingerprint" $ prop_fingerPrintRepParseIso
    ]
