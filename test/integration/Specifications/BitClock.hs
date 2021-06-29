{-# LANGUAGE OverloadedLabels #-}

module Specifications.BitClock where

import Data.Type.Rec (type (#))
import Data.Word (Word8)
import qualified Data.Set as Set
import Control.Exception (throwIO)

import Language.Spectacle
  ( define,
    always,
    eventually,
    modelCheck,
    plain,
    (.=),
    (/\),
  )
import Language.Spectacle.AST (Action, Initial, Invariant)
import Language.Spectacle.Spec.Base (Fairness (WeaklyFair))

type BitClock = '[ "clock" # Word8 ]

-- | Initial starting state of our bitclock.
initial :: Initial BitClock ()
initial = do
  #clock `define` return 0

action :: Action BitClock Bool
action = do

  clock <- plain #clock

  #clock .= do
    if clock == 0
      then return 1
      else return 0

  return True

invariant :: Invariant BitClock Bool
invariant = do
  clock <- plain #clock

  eventually (return (clock > 0))
    /\ always (return (Set.member clock (Set.fromList [0,1])))

check :: IO ()
check = case modelCheck initial action invariant Nothing WeaklyFair of
  (Left exc, _)
    -> throwIO exc
  _ -> pure ()
