{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE NegativeLiterals #-}
{-# LANGUAGE OverloadedLabels #-}

module Specifications.SimpleClock where

import Language.Spectacle
  ( Action,
    Initial,
    Invariant,
    always,
    defaultInteraction,
    define,
    eventually,
    modelCheck,
    plain,
    prime,
    weakFair,
    (.=),
    (/\),
    (==>),
    (\/),
    type (#),
  )
import Language.Spectacle.Specification
  ( Specification
      ( Specification,
        fairnessConstraint,
        initialAction,
        nextAction,
        temporalFormula,
        terminationFormula
      ),
  )

-- ---------------------------------------------------------------------------------------------------------------------

type ClockSpec = '["hours" # Int]

initial :: Initial ClockSpec ()
initial = do
  #hours `define` return 0

action :: Action ClockSpec Bool
action = tick \/ rollover

tick :: Action ClockSpec Bool
tick = do
  hours <- plain #hours
  #hours .= return (hours + 1)
  return (hours < 23)

rollover :: Action ClockSpec Bool
rollover = do
  hours <- plain #hours
  #hours .= return 0
  return (23 <= hours)

breakClock :: Action ClockSpec Bool
breakClock = do
  hours <- plain #hours
  #hours .= return hours
  return (0 <= hours)

formula :: Invariant ClockSpec Bool
formula = always inBounds /\ isZero ==> always (eventually willRollOver)
  where
    inBounds :: Invariant ClockSpec Bool
    inBounds = do
      hours <- plain #hours
      return (0 <= hours && hours <= 23)

    isZero :: Invariant ClockSpec Bool
    isZero = do
      hours <- plain #hours
      return (hours == 0)

    willRollOver = do
      hours <- plain #hours
      hours' <- prime #hours
      return (hours == 23 && hours' == 0)

check :: IO ()
check = do
  let spec :: Specification ClockSpec
      spec =
        Specification
          { initialAction = initial
          , nextAction = action
          , temporalFormula = formula
          , terminationFormula = Nothing
          , fairnessConstraint = weakFair
          }
  defaultInteraction (modelCheck spec)
