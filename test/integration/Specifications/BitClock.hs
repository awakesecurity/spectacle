{-# LANGUAGE OverloadedLabels #-}

module Specifications.BitClock where

import Data.Word (Word8)

import Language.Spectacle (Action, (.=), plain)
import Language.Spectacle.Interaction (defaultInteraction)
import Language.Spectacle.Specification
  ( Always,
    Fairness (WeakFair),
    Spec (Spec),
    Var ((:=)),
    VariableCtxt,
    type (!>) (WeakFairAction),
  )

-- ---------------------------------------------------------------------------------------------------------------------

type BitClockSpec = Spec (Var "clock" Word8) ("next" !> 'WeakFair) (Always "next")

next :: Action (VariableCtxt BitClockSpec) Bool
next = do
  clock <- plain #clock

  #clock
    .= if clock == 0
      then return 1
      else return 0

  return (clock == 0 || clock == 1)

spec :: BitClockSpec
spec = Spec (#clock := return 0) (WeakFairAction #next next)

check :: IO ()
check = defaultInteraction spec
