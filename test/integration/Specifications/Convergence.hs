{-# LANGUAGE OverloadedLabels #-}

-- |
--
-- @since 1.0.0
module Specifications.Convergence (
  interactSingleConvergenceSpec,
  singleConvergenceSpec,
  interactSplitConvergenceSpec,
  splitConvergenceSpec,
) where

import Control.Monad (guard)
import Data.Type.Rec (pattern ConF, pattern NilF, type (#))
import Language.Spectacle.AST (Action, Temporal)
import Language.Spectacle.Fairness (Fairness (WeakFair))
import Language.Spectacle.Interaction (interaction)
import Language.Spectacle.Specification (
  ActionType (ActionWF),
  Modality (Stays),
  Specification (..),
  TemporalType (PropFG),
 )
import Language.Spectacle.Syntax (oneOf, plain, (.=), exists, conjunct)

--------------------------------------------------------------------------------

type SingleActs =
  '[ "takeStep" # 'WeakFair
   ]

interactSingleConvergenceSpec :: IO ()
interactSingleConvergenceSpec = do 
  interaction singleConvergenceSpec

singleConvergenceSpec :: ConvergenceSpec SingleActs
singleConvergenceSpec =
  Specification
    { specInit =
        ConF #stepsLeft (pure 4)
          . ConF #position (pure 0)
          $ NilF
    , specNext =
        ConF #takeStep (ActionWF takeStepSingle) NilF
    , specProp =
        ConF #convergence (PropFG propConvergence) NilF
    }

--------------------------------------------------------------------------------

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
        ConF #stepsLeft (pure 4)
          . ConF #position (pure 0)
          $ NilF
    , specNext =
        ConF #stepNegative (ActionWF (takeStep pred))
          . ConF #stepPositive (ActionWF (takeStep succ))
          $ NilF
    , specProp =
        ConF #convergence (PropFG propConvergence) NilF
    }

--------------------------------------------------------------------------------

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
  '[ "convergence" # 'Stays
   ]

takeStepSingle :: Action ConvergenceVar Bool
takeStepSingle = do
  stepsLeft <- plain #stepsLeft
  position  <- plain #position

  guard (0 < stepsLeft)

  position' <- oneOf [position - 1, position + 1]

  guard (abs position' < stepsLeft)

  #position  .= pure position'
  #stepsLeft .= pure (pred stepsLeft)

  pure True

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

propConvergence :: Temporal ConvergenceVar Bool
propConvergence = do
  position  <- plain #position
  stepsLeft <- plain #stepsLeft 
  pure (position == 0 && stepsLeft `elem` [0, 1])

-- FIXME: exists does not properly weave NonDet choice
--   * Exists throws an exception, which should probably be empty
--   * Specification below is an example of unexpected behavior
--
-- takeStepSingle :: Action ConvergenceVar Bool
-- takeStepSingle = do
--   stepsLeft <- plain #stepsLeft
--   position  <- plain #position

--   guard (0 < stepsLeft)

--   let newPositions = [position - 1, position + 1]

--   exists newPositions \position' -> do 
--     #position  .= pure position'
--     #stepsLeft .= pure (pred stepsLeft)
--     pure (abs position' < stepsLeft)