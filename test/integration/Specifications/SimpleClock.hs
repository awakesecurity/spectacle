{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE NegativeLiterals #-}
{-# LANGUAGE OverloadedLabels #-}

module Specifications.SimpleClock where

import Control.Monad (when)

import Data.Type.Rec
import qualified Debug.Trace as Debug
import Language.Spectacle
import Language.Spectacle.AST.Temporal
import Language.Spectacle.Fairness
import Language.Spectacle.Model
import Language.Spectacle.Model.ModelError
import Language.Spectacle.Specification

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

clockSpecCheck :: IO ()
clockSpecCheck = do
  modelcheck clockSpec >>= \case
    Left err -> print err
    Right xs -> print xs
