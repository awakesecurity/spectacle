{-# LANGUAGE NegativeLiterals #-}
{-# LANGUAGE OverloadedLabels #-}

-- | Unit tests on detecting liveness properties within cyclic execution traces.
--
-- @since 0.1.0.0
module Spectacle.Checker.CyclicLiveness
  ( tests,
  )
where

import Hedgehog (Property, failure, property, success, withTests)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.Hedgehog (testProperty)

import Language.Spectacle
  ( Action,
    Initial,
    Invariant,
    define,
    eventually,
    modelCheck,
    plain,
    prime,
    weakFair,
    (.=),
    (\/),
    type (#),
  )
import Language.Spectacle.Checker.Metrics (ModelMetrics)
import Language.Spectacle.Checker.Model.MCError (MCError)
import Language.Spectacle.Specification (Specification (Specification))

-- ---------------------------------------------------------------------------------------------------------------------

type CyclicLivenessSpec =
  '[ "x" # Int
   , "s" # Bool
   ]

specInit :: Initial CyclicLivenessSpec ()
specInit = do
  #x `define` return 0
  #s `define` return True

specNextInfiniteExt :: Action CyclicLivenessSpec Bool
specNextInfiniteExt = specNextInc \/ specNextBadRollover \/ specNextBadInfiniteExt

specNextInfiniteCycle :: Action CyclicLivenessSpec Bool
specNextInfiniteCycle = specNextInc \/ specNextBadRollover \/ specNextBadInfiniteCycle

specNextInc :: Action CyclicLivenessSpec Bool
specNextInc = do
  x <- plain #x
  #x .= return (1 + x)
  (x /= 2 &&) <$> plain #s

specNextBadRollover :: Action CyclicLivenessSpec Bool
specNextBadRollover = do
  x <- plain #x
  #x .= return 1
  #s .= return False
  (x == 2 &&) <$> plain #s

specNextBadInfiniteExt :: Action CyclicLivenessSpec Bool
specNextBadInfiniteExt = do
  #x .= plain #x
  #s .= plain #s
  not <$> plain #s

specNextBadInfiniteCycle :: Action CyclicLivenessSpec Bool
specNextBadInfiniteCycle = do
  x <- plain #x
  if x == 1
    then #x .= return (- 1)
    else #x .= return 1
  not <$> plain #s

specProp :: Invariant CyclicLivenessSpec Bool
specProp = eventually do
  x <- plain #x
  x' <- prime #x
  return (x == 2 && x' == 0)

-- | 'testInfiniteCycleLiveness' tests that the model checker can detect liveness property violations in an infinite,
-- unchanging sequence extending the behavior. The 'Action' 'specNextInfiniteExt' generates the trace:
--
-- @
--     World 1                World 2                  World 3                  World n
--
--  'specNextInc'    ==>   'specNextInc'    ==>  'specNextBadRollover'  ==>  Unchanged ...
-- x = 0 --> x' = 1       x = 1 --> x' = 2         x = 2 --> x' = 1
-- s = T --> s' = T       s = T --> s' = T         s = T --> s' = F
-- @
--
-- Since @(x = 1 /\ s = F)@ for all worlds n, the liveness property @eventually (s = T && s = 2 && s' = 0)@ is violated.
-- Relevant issues:
-- * <https://github.com/awakesecurity/spectacle/issues/29>
--
-- @since 0.1.0.0
testInfiniteExtLiveness :: Property
testInfiniteExtLiveness = withTests 1 . property $ do
  case checkSpecInfiniteExt of
    Left _ -> success
    Right _ -> failure
  where
    checkSpecInfiniteExt :: Either [MCError CyclicLivenessSpec] ModelMetrics
    checkSpecInfiniteExt = modelCheck (Specification specInit specNextInfiniteExt specProp Nothing weakFair)

-- | 'testInfiniteCycleLiveness' tests that the model checker can detect liveness property violations in cycles
-- infinitely extending the behavior. The 'Action' 'specNextInfiniteCycle' generates the trace:
--
-- @
--     World 1                World 2                  World 3                       World 4               World n
--
--  'specNextInc'    ==>   'specNextInc'    ==>  'specNextBadRollover'  ==>  'specNextInfiniteCycle'  ==>    ...
-- x = 0 --> x' = 1       x = 1 --> x' = 2         x = 2 --> x' = 1             x = 1 --> x' = -1             |
-- s = T --> s' = T       s = T --> s' = T         s = T --> s' = F             s = F --> s' = F              |
--                                                        ^                                                   |
--                                                        |                                                   |
--                                                        +---------------------------------------------------+
-- @
--
-- The cycle here is generated by the sequence @World 3 -> World 4@, the model checker should emit a liveness property
-- violation for the property @eventually (s = T && s = 2 && s' = 0). Relevant issues:
-- * <https://github.com/awakesecurity/spectacle/issues/29>
--
-- @since 0.1.0.0
testInfiniteCycleLiveness :: Property
testInfiniteCycleLiveness = withTests 1 . property $ do
  case checkSpecInfiniteCycle of
    Left _ -> success
    Right _ -> failure
  where
    checkSpecInfiniteCycle :: Either [MCError CyclicLivenessSpec] ModelMetrics
    checkSpecInfiniteCycle = modelCheck (Specification specInit specNextInfiniteCycle specProp Nothing weakFair)

tests :: TestTree
tests =
  testGroup
    "Checker.CyclicLiveness"
    [ testProperty "infinite unchanging extensions checked by liveness properties" testInfiniteExtLiveness
    , testProperty "infinite cyclic extensions checked by liveness properties" testInfiniteCycleLiveness
    ]
