import Test.Tasty (defaultMain, testGroup, TestTree)
import Test.Tasty.HUnit (testCase)

-- import qualified Specifications.BitClock as BitClock
-- import qualified Specifications.SimpleClock as SimpleClock
import qualified Specifications.Diehard as Diehard
-- import qualified Specifications.SpanningTree as SpanningTree
-- import qualified Specifications.DijkstraMutex as DijkstraMutex

verify :: (String, IO ()) -> TestTree
verify (name, modelChecker) = testCase name do modelChecker

main :: IO ()
main =
  defaultMain $
    testGroup
      "integration tests"
      (verify <$> models)

  where
    models =
      [
      -- ("BitClock", BitClock.check)
      -- , ("Diehard",  Diehard.check)
      -- , ("SimpleClock", SimpleClock.check)
      -- , ("SpanningTree", SpanningTree.check)
      -- , ("DijkstraMutex", DijkstraMutex.check)
      ]
