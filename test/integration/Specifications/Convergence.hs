{-# LANGUAGE OverloadedLabels #-}

-- |
--
-- @since 1.0.0
module Specifications.Convergence
  ( interactSingleConvergenceSpec,
    interactSplitConvergenceSpec,
  )
where

import Control.Monad (guard)
import Language.Spectacle

------------------------------------------------------------------------------------------------------------------------

type SingleActs =
  '[ "takeStep" # 'WeakFair
   ]

interactSingleConvergenceSpec :: IO ()
interactSingleConvergenceSpec = interaction singleConvergenceSpec

singleConvergenceSpec :: ConvergenceSpec SingleActs
singleConvergenceSpec =
  Specification
    { specInit =
        ConF #stepsLeft (pure 6)
          . ConF #position (pure 0)
          $ NilF
    , specNext =
        ConF #takeStep (ActionWF (oneOf [succ, pred] >>= takeStep)) NilF
    , specProp =
        ConF #positionZero (PropFG positionZero) NilF
    }

------------------------------------------------------------------------------------------------------------------------

type SplitActs =
  '[ "stepNegative" # 'WeakFair
   , "stepPositive" # 'WeakFair
   ]

interactSplitConvergenceSpec :: IO ()
interactSplitConvergenceSpec = interaction splitConvergenceSpec

splitConvergenceSpec :: ConvergenceSpec SplitActs
splitConvergenceSpec =
  Specification
    { specInit =
        ConF #stepsLeft (pure 6)
          . ConF #position (pure 0)
          $ NilF
    , specNext =
        ConF #stepNegative (ActionWF (takeStep pred))
          . ConF #stepPositive (ActionWF (takeStep succ))
          $ NilF
    , specProp =
        ConF #positionZero (PropFG positionZero) NilF
    }

------------------------------------------------------------------------------------------------------------------------

type ConvergenceSpec acts =
  Specification
    ConvergenceVar
    acts
    ConvergenceProp

type ConvergenceVar =
  '[ "stepsLeft" # Int
   , "position" # Int
   ]

type ConvergenceProp =
  '[ "positionZero" # 'Stays
   ]

takeStep :: (Int -> Int) -> Action ConvergenceVar Bool
takeStep f = do
  stepsLeft <- plain #stepsLeft
  position <- plain #position
  guard (0 < stepsLeft)

  let position' = f position
  guard (abs position' < stepsLeft)

  #position .= pure position'
  #stepsLeft .= pure (pred stepsLeft)

  pure True

positionZero :: Temporal ConvergenceVar Bool
positionZero = do
  position <- plain #position
  pure (position == 0)
