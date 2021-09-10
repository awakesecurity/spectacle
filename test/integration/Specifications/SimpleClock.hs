{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE NegativeLiterals #-}
{-# LANGUAGE OverloadedLabels #-}

module Specifications.SimpleClock where

import Control.Applicative ()
import Control.Monad (when)

import Language.Spectacle (Action, (.=), plain)
import Language.Spectacle.Interaction (defaultInteraction)
import Language.Spectacle.Specification
  ( Always,
    Eventually,
    Fairness (WeakFair),
    Spec(Spec),
    Var((:=)),
    type VariableCtxt,
    type (!>)(WeakFairAction),
    type (/\),
    type (\/)((:\/:)),
  )

-- ---------------------------------------------------------------------------------------------------------------------

type ClockSpec =
  Spec
    (Var "hours" Int)
    ("tick" !> 'WeakFair \/ "rollover" !> 'WeakFair)
    (Always "tick" /\ Eventually "rollover")

tick :: Action (VariableCtxt ClockSpec) Bool
tick = do
  hours <- plain #hours

  when (hours < 23) do
    #hours .= return (1 + hours)

  return (0 <= hours && hours <= 23)

rollover :: Action (VariableCtxt ClockSpec) Bool
rollover = do
  hours <- plain #hours
  #hours .= return 0
  return (hours == 23)

spec :: ClockSpec
spec = Spec (#hours := return 0) specNext
  where
    specNext = WeakFairAction #tick tick :\/: WeakFairAction #rollover rollover

check :: IO ()
check = defaultInteraction spec
