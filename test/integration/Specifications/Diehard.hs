{-# LANGUAGE OverloadedLabels #-}

module Specifications.Diehard where

import Data.Type.Rec (type (#))
import Language.Spectacle
  ( always,
    define,
    modelCheck,
    plain,
    prime,
    (.=),
    (/\),
    (\/),
  )
import Language.Spectacle.AST (Action, Initial, Invariant, Terminate)
import Language.Spectacle.Spec.Base (Fairness (Unfair))

import Control.Exception (throwIO)
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
  fillSmallJug
    \/ fillBigJug
    \/ emptySmallJug
    \/ emptyBigJug
    \/ smallToBig
    \/ bigToSmall
  where
    fillSmallJug = #smallJug .= return 3 >> return True

    fillBigJug = #bigJug .= return 5 >> return True

    emptySmallJug = #smallJug .= return 0 >> return True

    emptyBigJug = #bigJug .= return 0 >> return True

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

invariant :: Invariant Diehard Bool
invariant = do
  always smallJugBounds /\ always bigJugBounds
  where
    smallJugBounds = do
      smallJug <- plain #smallJug
      return (0 <= smallJug && smallJug <= 3)

    bigJugBounds = do
      bigJug <- plain #bigJug
      return (0 <= bigJug && bigJug <= 5)

terminate :: Terminate Diehard Bool
terminate = do
  bigJug <- plain #bigJug
  return (bigJug == 4)

check :: IO ()
check = case modelCheck initial next invariant (Just terminate) Unfair of
  (Left exc, _)
    -> throwIO exc
  _ -> pure ()
