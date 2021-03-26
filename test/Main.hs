import Test.Tasty (defaultMain, testGroup)

import qualified Fingerprint
import qualified Fingerprint.Array

main :: IO ()
main =
  defaultMain $
    testGroup
      "unit tests"
      [ Fingerprint.tests
      , Fingerprint.Array.tests
      ]
