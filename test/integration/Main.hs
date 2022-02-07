import Control.Monad.IO.Class (liftIO)
import Data.Hashable (Hashable)

import Hedgehog (Property, annotateShow, failure, property, success, withTests)
import Test.Tasty (defaultMain, testGroup)
import Test.Tasty.Hedgehog (testProperty)

import Data.Type.Rec (HasDict)
import Language.Spectacle.Model (modelcheck)
import Language.Spectacle.Specification (Specification)

import qualified Specifications.BitClock as BitClock
import qualified Specifications.Diehard as Diehard
import qualified Specifications.RateLimit as RateLimit
import qualified Specifications.SimpleClock as SimpleClock
import qualified Specifications.Status as Status

-- ---------------------------------------------------------------------------------------------------------------------

main :: IO ()
main =
  defaultMain $
    testGroup
      "Integration"
      [ testProperty "Specifications.BitClock" (testCheckVerify BitClock.bitClockSpec)
      , testProperty "Specifications.Diehard" (testCheckRefute Diehard.diehardSpec)
      , testProperty "Specifications.RateLimit" (testCheckVerify RateLimit.rateLimitSpec)
      , testProperty "Specifications.SimpleClock" (testCheckVerify SimpleClock.clockSpec)
      , testProperty "Specifications.Status" (testCheckVerify Status.statusSpec)
      ]

testCheckVerify ::
  (HasDict Hashable ctx, HasDict Show ctx) =>
  Specification ctx acts form ->
  Property
testCheckVerify spec =
  withTests 1 $ property do
    checkResult <- liftIO (modelcheck spec)
    case checkResult of
      Left err -> annotateShow err >> failure
      Right {} -> success

testCheckRefute ::
  HasDict Hashable ctx =>
  Specification ctx acts form ->
  Property
testCheckRefute spec =
  withTests 1 $ property do
    checkResult <- liftIO (modelcheck spec)
    case checkResult of
      Left {} -> success
      Right {} -> failure
