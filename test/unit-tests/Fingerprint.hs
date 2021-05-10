module Fingerprint
  ( tests,
  )
where

import Data.Hashable (Hashable, hash)
import GHC.Exts (fromList)
import Hedgehog (MonadGen, Property, Range, assert, forAll, property)
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.Hedgehog (testProperty)

import Data.Fingerprint (Fingerprint, insert, member)

-- -----------------------------------------------------------------------------

-- | Generate a 'Fingerprint' structure.
genFingerprint :: (Hashable a, MonadGen m) => Range Int -> m a -> m Fingerprint
genFingerprint range gen =
  fromList <$> Gen.list range (fromIntegral . hash <$> gen)

-- | Generate a value to hash.
genHash :: MonadGen m => m String
genHash = Gen.string (Range.constant 0 10) Gen.alphaNum

-- | Making sure fingerprint never drops a hash.
prop_insert_lookup :: Property
prop_insert_lookup = property do
  xs <- forAll $ Gen.list (Range.constant 0 10) genHash
  fps <- forAll $ genFingerprint (Range.constant 0 10) genHash
  let fps' = foldl (flip insert) fps $ map (fromIntegral . hash) xs
  mapM_ (assert . (`member` fps')) xs

tests :: TestTree
tests =
  testGroup
    "Data.Fingerprint"
    [ testProperty "insertion and lookup" $ prop_insert_lookup
    ]
