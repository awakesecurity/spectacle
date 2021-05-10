import Test.Tasty (defaultMain, testGroup)

-- import qualified Fingerprint
-- import qualified Fingerprint.Array
import qualified Spectacle.Syntax.NonDet
-- import qualified Spectacle.Syntax.Logic
import qualified Spectacle.Syntax.Modal

main :: IO ()
main =
  defaultMain $
    testGroup
      "unit tests"
      [ -- Fingerprint.tests
        -- , Fingerprint.Array.tests
        -- Spectacle.Syntax.Logic.tests
        Spectacle.Syntax.Modal.tests
      , Spectacle.Syntax.NonDet.tests
      ]
