import Test.Tasty (defaultMain, testGroup)

import qualified Checker
import qualified Fingerprint
import qualified Fingerprint.Array
import qualified Lang.Sum
import qualified Spec

main :: IO ()
main =
  defaultMain $
    testGroup
      "unit tests"
      [ Checker.tests
      , Lang.Sum.tests
      , Fingerprint.tests
      , Fingerprint.Array.tests
      , Spec.tests
      ]
