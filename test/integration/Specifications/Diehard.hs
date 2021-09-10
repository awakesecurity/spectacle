{-# LANGUAGE OverloadedLabels #-}

module Specifications.Diehard where

import Language.Spectacle (Action, plain, prime, (.=))
import Language.Spectacle.Interaction (defaultInteraction)
import Language.Spectacle.Specification
  ( Always,
    Eventually,
    Fairness (WeakFair),
    Spec (Spec),
    Var ((:=)),
    VariableCtxt,
    type (!>) (WeakFairAction),
    type (/\),
    type (:.) ((:.)),
    type (\/) ((:\/:)),
  )

-- -------------------------------------------------------------------------------------------------

type DiehardSpec =
  Spec
    ( Var "smallJug" Int
        :. Var "bigJug" Int
    )
    ( ("emptySmall" !> 'WeakFair)
        \/ ("emptyBig" !> 'WeakFair)
        \/ ("fillSmall" !> 'WeakFair)
        \/ ("fillBig" !> 'WeakFair)
        \/ ("smallToBig" !> 'WeakFair)
        \/ ("bigToSmall" !> 'WeakFair)
        \/ ("isSolution" !> 'WeakFair)
    )
    ( Always '["emptySmall", "emptyBig", "fillSmall", "fillBig", "smallToBig", "bigToSmall"]
        /\ Eventually "isSolution"
    )

emptySmall :: Action (VariableCtxt DiehardSpec) Bool
emptySmall = do
  #smallJug .= return 0
  #bigJug .= plain #bigJug
  return True

emptyBig :: Action (VariableCtxt DiehardSpec) Bool
emptyBig = do
  #smallJug .= plain #smallJug
  #bigJug .= return 0
  return True

fillSmall :: Action (VariableCtxt DiehardSpec) Bool
fillSmall = do
  #smallJug .= return 3
  #bigJug .= plain #bigJug
  return True

fillBig :: Action (VariableCtxt DiehardSpec) Bool
fillBig = do
  #smallJug .= plain #smallJug
  #bigJug .= return 5
  return True

smallToBig :: Action (VariableCtxt DiehardSpec) Bool
smallToBig = do
  smallJug <- plain #smallJug
  bigJug <- plain #bigJug

  #smallJug .= do
    bigJug' <- prime #bigJug
    return (smallJug - (bigJug' - bigJug))
  #bigJug .= return (min (bigJug + smallJug) 5)
  return True

bigToSmall :: Action (VariableCtxt DiehardSpec) Bool
bigToSmall = do
  smallJug <- plain #smallJug
  bigJug <- plain #bigJug

  #smallJug .= return (min (bigJug + smallJug) 3)
  #bigJug .= do
    smallJug' <- prime #smallJug
    return (bigJug - (smallJug' - smallJug))
  return True

isSolution :: Action (VariableCtxt DiehardSpec) Bool
isSolution = do
  bigJug <- plain #bigJug
  return (bigJug == 4)

spec :: DiehardSpec
spec = Spec specInit specNext
  where
    specInit =
      (#smallJug := return 0)
        :. (#bigJug := return 0)

    specNext =
      WeakFairAction #emptySmall emptySmall
        :\/: WeakFairAction #emptyBig emptyBig
        :\/: WeakFairAction #fillSmall fillSmall
        :\/: WeakFairAction #fillBig fillBig
        :\/: WeakFairAction #smallToBig smallToBig
        :\/: WeakFairAction #bigToSmall bigToSmall
        :\/: WeakFairAction #isSolution isSolution

check :: IO ()
check = defaultInteraction spec
