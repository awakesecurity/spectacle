{-# LANGUAGE OverloadedLabels #-}

module Specifications.Diehard where

import Debug.Trace as Debug
import Data.Type.Rec
import Language.Spectacle.AST.Action
import Language.Spectacle.AST.Temporal
import Language.Spectacle.Fairness
import Language.Spectacle.Model
import Language.Spectacle.Specification
import Language.Spectacle.Syntax

-- -------------------------------------------------------------------------------------------------

type DiehardSpec =
  Specification
    DiehardVars
    '[ "emptySmall" # 'Unfair
     , "emptyBig" # 'Unfair
     , "fillSmall" # 'Unfair
     , "fillBig" # 'Unfair
     , "smallToBig" # 'Unfair
     , "bigToSmall" # 'Unfair
     ]
    '[ "isSolved" # 'Always
     ]

type DiehardVars =
  '[ "smallJug" # Int
   , "bigJug" # Int
   ]

emptySmall :: Action DiehardVars Bool
emptySmall = do
  #smallJug .= pure 0
  return True

emptyBig :: Action DiehardVars Bool
emptyBig = do
  #bigJug .= pure 0
  pure True

fillSmall :: Action DiehardVars Bool
fillSmall = do
  #smallJug .= pure 3
  return True

fillBig :: Action DiehardVars Bool
fillBig = do
  #bigJug .= pure 5
  pure True

bigToSmall :: Action DiehardVars Bool
bigToSmall = do
  bigJug <- plain #bigJug
  smallJug <- plain #smallJug

  #smallJug .= pure (min (bigJug + smallJug) 3)
  #bigJug .= do
    smallJug' <- prime #smallJug
    pure (bigJug - (smallJug' - smallJug))

  pure True

smallToBig :: Action DiehardVars Bool
smallToBig = do
  bigJug <- plain #bigJug
  smallJug <- plain #smallJug

  #bigJug .= pure (min (bigJug + smallJug) 5)
  #smallJug .= do
    bigJug' <- prime #bigJug
    pure (smallJug - (bigJug' - bigJug))

  pure True

isSolved :: Temporal DiehardVars Bool
isSolved = do
  bigJug <- plain #bigJug
  pure (bigJug /= 4)

diehardSpec :: DiehardSpec
diehardSpec =
  Specification
    { specInit =
        ConF #smallJug (pure 0)
          . ConF #bigJug (pure 0)
          $ NilF
    , specNext =
        ConF #emptySmall (ActionUF emptySmall)
          . ConF #emptyBig (ActionUF emptyBig)
          . ConF #fillSmall (ActionUF fillSmall)
          . ConF #fillBig (ActionUF fillBig)
          . ConF #smallToBig (ActionUF smallToBig)
          . ConF #bigToSmall (ActionUF bigToSmall)
          $ NilF
    , specProp =
        ConF #isSolved (PropG isSolved) NilF
    }


diehardSpecCheck :: IO ()
diehardSpecCheck = do
  modelcheck diehardSpec >>= \case
    Left err -> print err
    Right xs -> print xs

