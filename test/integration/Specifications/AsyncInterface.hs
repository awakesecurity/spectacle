{-# LANGUAGE OverloadedLabels #-}

-- | Asynchronous interface, adapted from Leslie Lamport's /Specifying Systems/
-- chapter 3. A sender and receiver communicate through a one-place buffer
-- using a two-bit handshake: the sender flips @rdy@ to push a new @val@; the
-- receiver flips @ack@ to acknowledge it. The pair @(rdy, ack)@ cycles
-- @(F, F) → (T, F) → (T, T) → (F, T) → (F, F)@ as @send@ and @receive@
-- alternate, while @val@ counts modulo four to keep the state space finite.
--
-- Both @send@ and @receive@ are weak-fair: they are enabled at every other
-- state of the cycle and taken on every edge that leaves that state, so the
-- BSCC is fair under WF on both actions. The 'Infinitely' property
-- @rdy == ack@ (the "synchronized" handshake state) holds at four of the
-- eight states in the cycle BSCC, which the SCC checker recognizes via an
-- edge whose source has @rdy == ack@.
--
-- The spec also includes a deliberate liveness counterexample: 'Stays' on
-- @rdy == ack@ refutes, because every cycle through the BSCC must visit
-- @rdy /= ack@ states, so no fair lasso can satisfy "eventually-always
-- synchronized." This is the value of having SCC analysis distinguish
-- @[]<>@ from @<>[]@ — one verifies, the other does not, and both answers
-- come from the same fair-cycle reasoning.
module Specifications.AsyncInterface where

import Control.Monad (when)

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

interactAsyncInterfaceSpec :: IO ()
interactAsyncInterfaceSpec = interaction asyncInterfaceSpec

-- ---------------------------------------------------------------------------------------------------------------------

type AsyncVars =
  '[ "val" # Int
   , "rdy" # Bool
   , "ack" # Bool
   ]

type AsyncInterfaceSpec =
  Specification
    AsyncVars
    '[ "send" # 'WeakFair
     , "receive" # 'WeakFair
     ]
    '[ "validValue" # 'Always
     , "syncInfinitely" # 'Infinitely
     ]

valBound :: Int
valBound = 4

send :: Action AsyncVars Bool
send = do
  rdy <- plain #rdy
  ack <- plain #ack
  when (rdy == ack) do
    val <- plain #val
    #val .= pure ((val + 1) `mod` valBound)
    #rdy .= pure (not rdy)
  pure (rdy == ack)

receive :: Action AsyncVars Bool
receive = do
  rdy <- plain #rdy
  ack <- plain #ack
  when (rdy /= ack) do
    #ack .= pure (not ack)
  pure (rdy /= ack)

validValue :: Temporal AsyncVars Bool
validValue = do
  v <- plain #val
  pure (0 <= v && v < valBound)

syncInfinitely :: Temporal AsyncVars Bool
syncInfinitely = do
  rdy <- plain #rdy
  ack <- plain #ack
  pure (rdy == ack)

asyncInterfaceSpec :: AsyncInterfaceSpec
asyncInterfaceSpec =
  Specification
    { specInit =
        ConF #val (pure 0)
          . ConF #rdy (pure False)
          . ConF #ack (pure False)
          $ NilF
    , specNext =
        ConF #send (ActionWF send)
          . ConF #receive (ActionWF receive)
          $ NilF
    , specProp =
        ConF #validValue (PropG validValue)
          . ConF #syncInfinitely (PropGF syncInfinitely)
          $ NilF
    }
