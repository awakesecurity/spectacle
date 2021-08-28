{-# LANGUAGE OverloadedLabels #-}

module Specifications.Diehard where

import Language.Spectacle
  ( Action,
    Initial,
    Invariant,
    Terminate,
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
import Debug.Trace

-- -------------------------------------------------------------------------------------------------

type Diehard =
  '[ "smallJug" # Int
   , "bigJug" # Int
   ]

initial :: Initial Diehard ()
initial = do
  #smallJug `define` pure 0
  #bigJug `define` pure 0

next :: Action Diehard Bool
next = do
  bigJug <- plain #bigJug
  smallJug <- plain #smallJug

  fillSmallJug
    \/ fillBigJug
    \/ emptySmallJug
    \/ emptyBigJug
    \/ smallToBig
    \/ bigToSmall
  where
    fillSmallJug = do
      #smallJug .= return 3
      #bigJug .= plain #bigJug
      return True

    fillBigJug = do
      #bigJug .= return 5
      #smallJug .= plain #smallJug
      return True

    emptySmallJug = do
      #smallJug .= return 0
      #bigJug .= plain #bigJug
      return True

    emptyBigJug = do
      #bigJug .= return 0
      #smallJug .= plain #smallJug
      return True

    smallToBig = do
      smallJug <- plain #smallJug
      bigJug <- plain #bigJug
      #smallJug .= do
        bigJug' <- prime #bigJug
        return (smallJug - (bigJug' - bigJug))
      #bigJug .= return (min (bigJug + smallJug) 5)
      return True

    bigToSmall = do
      smallJug <- plain #smallJug
      bigJug <- plain #bigJug
      #smallJug .= return (min (bigJug + smallJug) 3)
      #bigJug .= do
        smallJug' <- prime #smallJug
        return (bigJug - (smallJug' - smallJug))
      return True

formula :: Invariant Diehard Bool
formula = do
  eventually solved
  where
    -- smallJugBounds = do
    --   smallJug <- plain #smallJug
    --   return (0 <= smallJug && smallJug <= 3)

    -- bigJugBounds = do
    --   bigJug <- plain #bigJug
    --   return (0 <= bigJug && bigJug <= 5)

    solved = do
      bigJug <- plain #bigJug
      return (bigJug == 4)

termination :: Terminate Diehard Bool
termination = do
  bigJug <- plain #bigJug
  return (bigJug == 4)

check :: IO ()
check = do
  let spec :: Specification Diehard
      spec =
        Specification
          { initialAction = initial
          , nextAction = next
          , temporalFormula = formula
          , terminationFormula = Nothing
          , fairnessConstraint = weakFair
          }
  defaultInteraction (modelCheck spec)
