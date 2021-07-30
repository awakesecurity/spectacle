{-# LANGUAGE OverloadedLabels #-}

module Specifications.BitClock where

import Data.Type.Rec (type (#))
import Data.Word (Word8)
import qualified Data.Set as Set

import Language.Spectacle
  ( Action,
    Initial,
    Invariant,
    always,
    defaultInteraction,
    define,
    modelCheck,
    plain,
    weakFair,
    (.=),
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

type BitClock = '[ "clock" # Word8 ]

-- | Initial starting state of our bitclock.
initial :: Initial BitClock ()
initial = do
  #clock `define` return 0

next :: Action BitClock Bool
next = do

  clock <- plain #clock

  #clock .= do
    if clock == 0
      then return 1
      else return 0

  return True

formula :: Invariant BitClock Bool
formula = do
  clock <- plain #clock

  always (return (Set.member clock (Set.fromList [0,1])))

check :: IO ()
check = do

  let spec :: Specification BitClock
      spec =
        Specification
          { initialAction = initial
          , nextAction = next
          , temporalFormula = formula
          , terminationFormula = Nothing
          , fairnessConstraint = weakFair
          }

  defaultInteraction (modelCheck spec)
