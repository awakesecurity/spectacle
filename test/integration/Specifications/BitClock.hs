{-# LANGUAGE OverloadedLabels #-}

module Specifications.BitClock where

import Data.Word (Word8)

import Data.Functor.Tree
import Data.Type.Rec
import Data.World
import Language.Spectacle.AST.Action
import Language.Spectacle.AST.Temporal
import Language.Spectacle.Fairness
import Language.Spectacle.Model
import Language.Spectacle.Model.ModelError
import Language.Spectacle.Specification
import Language.Spectacle.Syntax

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

-- check :: IO ()
-- check = do
--   modelcheck bitClockSpec >>= \case
--     Left err -> print "error!"
--     Right xs -> print xs
