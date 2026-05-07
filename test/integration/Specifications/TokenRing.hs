{-# LANGUAGE OverloadedLabels #-}

-- | Token-ring spec — a classic TLA+ liveness exercise. Three nodes are
-- arranged in a ring and a single token cycles around. Under @WeakFair@ on
-- @pass@, the @pass@ action is enabled at every state of the cycle BSCC and
-- gets taken on every edge, so each node holds the token infinitely often.
--
-- This spec exists primarily to exercise the SCC fair-cycle checker for
-- 'Infinitely': the state graph is a non-trivial 3-state BSCC, and verifying
-- @[]<> (holder == k)@ for each @k@ requires the model checker to accept the
-- cycle as a fair lasso. The pre-SCC tree-walking @checkInfinitely@ would
-- refute @\"node2Holds\"@ here because its @cyclically@ search for @holder=2@
-- in the tree's "fairness exhausted" prefix gave up before reaching the next
-- @holder=2@ occurrence.
module Specifications.TokenRing where

import Language.Spectacle
  ( Action,
    ActionType (ActionWF),
    Fairness (WeakFair),
    Modality (Always, Infinitely),
    Specification (Specification),
    Temporal,
    TemporalType (PropG, PropGF),
    interaction,
    plain,
    specInit,
    specNext,
    specProp,
    (.=),
    pattern ConF,
    pattern NilF,
    type (#),
  )

-- ---------------------------------------------------------------------------------------------------------------------

interactTokenRingSpec :: IO ()
interactTokenRingSpec = interaction tokenRingSpec

-- ---------------------------------------------------------------------------------------------------------------------

type TokenRingVars = '["holder" # Int]

type TokenRingSpec =
  Specification
    TokenRingVars
    '["pass" # 'WeakFair]
    '[ "validHolder" # 'Always
     , "node0Holds" # 'Infinitely
     , "node1Holds" # 'Infinitely
     , "node2Holds" # 'Infinitely
     ]

ringSize :: Int
ringSize = 3

passToken :: Action TokenRingVars Bool
passToken = do
  h <- plain #holder
  #holder .= pure ((h + 1) `mod` ringSize)
  pure True

validHolder :: Temporal TokenRingVars Bool
validHolder = do
  h <- plain #holder
  pure (0 <= h && h < ringSize)

holderIs :: Int -> Temporal TokenRingVars Bool
holderIs n = do
  h <- plain #holder
  pure (h == n)

tokenRingSpec :: TokenRingSpec
tokenRingSpec =
  Specification
    { specInit = ConF #holder (pure 0) NilF
    , specNext = ConF #pass (ActionWF passToken) NilF
    , specProp =
        ConF #validHolder (PropG validHolder)
          . ConF #node0Holds (PropGF (holderIs 0))
          . ConF #node1Holds (PropGF (holderIs 1))
          . ConF #node2Holds (PropGF (holderIs 2))
          $ NilF
    }
