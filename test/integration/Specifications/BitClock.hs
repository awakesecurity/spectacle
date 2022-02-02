{-# LANGUAGE OverloadedLabels #-}

module Specifications.BitClock where

import Data.Word (Word8)

import Language.Spectacle
  ( Action,
    ActionType (ActionWF),
    Fairness (WeakFair),
    Modality (Always),
    Specification (Specification),
    Temporal,
    TemporalType (PropG),
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

check :: IO ()
check = interaction bitClockSpec

-- ---------------------------------------------------------------------------------------------------------------------

type BitClockSpec =
  Specification
    '["clock" # Word8]
    '["tick" # 'WeakFair]
    '["times" # 'Always]

bitClockNext :: Action '["clock" # Word8] Bool
bitClockNext = do
  clock <- plain #clock
  if clock == 0
    then #clock .= pure 1
    else #clock .= pure 0
  return True

bitClockTimes :: Temporal '["clock" # Word8] Bool
bitClockTimes = do
  clock <- plain #clock
  pure (clock == 0 || clock == 1)

bitClockSpec :: BitClockSpec
bitClockSpec =
  Specification
    { specInit = ConF #clock (pure 0) NilF
    , specNext = ConF #tick (ActionWF bitClockNext) NilF
    , specProp = ConF #times (PropG bitClockTimes) NilF
    }
