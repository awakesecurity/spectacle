{-# LANGUAGE OverloadedLabels #-}

module Specifications.SimpleClock where

import Language.Spectacle
  ( Action,
    ActionType (ActionWF),
    Fairness (WeakFair),
    Modality (Always, Infinitely),
    Specification (Specification),
    Temporal,
    TemporalType (PropG, PropGF),
    interaction,
    modelcheck,
    plain,
    prime,
    specInit,
    specNext,
    specProp,
    (.=),
    pattern ConF,
    pattern NilF,
    type (#),
  )

-- ---------------------------------------------------------------------------------------------------------------------

interactClockSpec :: IO ()
interactClockSpec = interaction clockSpec

clockSpecCheck :: IO ()
clockSpecCheck = do
  modelcheck clockSpec >>= \case
    Left err -> print err
    Right xs -> print xs

-- ---------------------------------------------------------------------------------------------------------------------

type ClockSpec =
  Specification
    '["hour" # Int]
    '["next" # 'WeakFair]
    '["ticks" # 'Infinitely, "times" # 'Always]

clockNext :: Action '["hour" # Int] Bool
clockNext = do
  hour <- plain #hour
  if hour == 12
    then #hour .= pure 1
    else #hour .= pure (1 + hour)
  pure True

clockTicks :: Temporal '["hour" # Int] Bool
clockTicks = do
  hour <- plain #hour
  hour' <- prime #hour
  pure (1 + hour == hour')

clockTimes :: Temporal '["hour" # Int] Bool
clockTimes = do
  hour <- plain #hour
  pure (1 <= hour && hour <= 12)

clockSpec :: ClockSpec
clockSpec =
  Specification
    { specInit = ConF #hour (pure 1) NilF
    , specNext = ConF #next (ActionWF clockNext) NilF
    , specProp =
        ConF #ticks (PropGF clockTicks)
          . ConF #times (PropG clockTimes)
          $ NilF
    }
