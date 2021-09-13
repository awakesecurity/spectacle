import Hedgehog (Property, property, annotateShow, success, failure)
import Test.Tasty (defaultMain, testGroup)
import Test.Tasty.Hedgehog (testProperty)

import Data.Type.Rec (Rec)
import Language.Spectacle.Checker.MCError (MCError)
import Language.Spectacle.Checker.MCMetrics (MCMetrics)

import qualified Specifications.BitClock as BitClock
import qualified Specifications.Diehard as Diehard
import qualified Specifications.SimpleClock as SimpleClock
import qualified Specifications.DijkstraMutex as DijkstraMutex

verify :: Show (Rec ctxt) => Either [MCError ctxt] MCMetrics -> Property
verify result = property do
  case result of
    Left errs -> do
      annotateShow errs
      failure
    Right _ -> success

main :: IO ()
main =
  defaultMain $
    testGroup
      "integration tests"
      [ testProperty "BitClock" (verify BitClock.test)
      , testProperty "Diehard" (verify Diehard.test)
      , testProperty "SimpleClock" (verify SimpleClock.test)
      , testProperty "DijkstraMutex" (verify DijkstraMutex.test)
      ]
