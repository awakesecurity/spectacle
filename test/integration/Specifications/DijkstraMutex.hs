{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE RecordWildCards #-}

module Specifications.DijkstraMutex where

import Control.Applicative ((<|>))
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap
import qualified Data.List as List

import Language.Spectacle
  ( Action,
    exists,
    forall,
    oneOf,
    plain,
    (.=),
  )
import Language.Spectacle.Interaction (defaultInteraction)
import Language.Spectacle.Checker.MCError (MCError)
import Language.Spectacle.Checker.MCMetrics (MCMetrics)
import Language.Spectacle.Checker (modelCheck)
import Language.Spectacle.Specification
  ( Always,
    Fairness (WeakFair),
    Spec (Spec),
    Var ((:=)),
    VariableCtxt,
    type (!>) (WeakFairAction),
    type (:.) ((:.)),
    type (\/) ((:\/:)),
  )

import Specifications.DijkstraMutex.Process
  ( Process (Process),
    ProcessLabel (ProcessLabel),
    StepLabel (Critical, Li0, Li1, Li2, Li3a, Li3b, Li3c, Li4a, Li4b, Li5, Li6, Noncritical),
    iterState,
    pix,
    procStep,
  )

-- ---------------------------------------------------------------------------------------------------------------------

data Constants = Constants
  { numProcesses :: Int
  , processSet :: [ProcessLabel]
  }

type DijkstraMutexSpec =
  Spec
    ( Var "isLooping" (HashMap ProcessLabel Bool)
        :. Var "goCritical" (HashMap ProcessLabel Bool)
        :. Var "procIx" ProcessLabel
        :. Var "procs" (HashMap ProcessLabel Process)
    )
    ( "next" !> 'WeakFair
        \/ "mutualExclusion" !> 'WeakFair
    )
    ( Always "next"
    )

next ::
  (?constants :: Constants) =>
  Action (VariableCtxt DijkstraMutexSpec) Bool
next = do
  let Constants {..} = ?constants

  exists processSet \pid -> do
    nextLi0 pid
      <|> nextLi1 pid
      <|> nextLi2 pid
      <|> nextLi3a pid
      <|> nextLi3b pid
      <|> nextLi3c pid
      <|> nextLi4a pid
      <|> nextLi4b pid
      <|> nextCrit pid
      <|> nextLi5 pid
      <|> nextLi6 pid
      <|> nextNoncrit pid

nextLi0 :: ProcessLabel -> Action (VariableCtxt DijkstraMutexSpec) Bool
nextLi0 pid = do
  Process {..} <- (HashMap.! pid) <$> plain #procs
  #isLooping .= (HashMap.insert pid False <$> plain #isLooping)
  #procs .= (HashMap.insert pid Process {procStep = Li1, ..} <$> plain #procs)
  return (procStep == Li0)

nextLi1 :: ProcessLabel -> Action (VariableCtxt DijkstraMutexSpec) Bool
nextLi1 pid = do
  Process {..} <- (HashMap.! pid) <$> plain #procs
  procIx <- plain #procIx

  if procIx /= pid
    then #procs .= (HashMap.insert pid Process {procStep = Li2, ..} <$> plain #procs)
    else #procs .= (HashMap.insert pid Process {procStep = Li4a, ..} <$> plain #procs)

  return (procStep == Li1)

nextLi2 :: ProcessLabel -> Action (VariableCtxt DijkstraMutexSpec) Bool
nextLi2 pid = do
  Process {..} <- (HashMap.! pid) <$> plain #procs

  #goCritical .= (HashMap.insert pid True <$> plain #goCritical)
  #procs .= (HashMap.insert pid Process {procStep = Li3a, ..} <$> plain #procs)

  return (procStep == Li2)

nextLi3a :: ProcessLabel -> Action (VariableCtxt DijkstraMutexSpec) Bool
nextLi3a pid = do
  Process {..} <- (HashMap.! pid) <$> plain #procs
  procIx <- plain #procIx

  #procs .= (HashMap.insert pid Process {procStep = Li3b, pix = procIx, ..} <$> plain #procs)

  return (procStep == Li3a)

nextLi3b :: ProcessLabel -> Action (VariableCtxt DijkstraMutexSpec) Bool
nextLi3b pid = do
  Process {..} <- (HashMap.! pid) <$> plain #procs
  isLooping <- (HashMap.! pix) <$> plain #isLooping

  if isLooping
    then #procs .= (HashMap.insert pid Process {procStep = Li3c, ..} <$> plain #procs)
    else #procs .= (HashMap.insert pid Process {procStep = Li1, ..} <$> plain #procs)

  return (procStep == Li3b)

nextLi3c :: ProcessLabel -> Action (VariableCtxt DijkstraMutexSpec) Bool
nextLi3c pid = do
  Process {..} <- (HashMap.! pid) <$> plain #procs

  #procIx .= return pid
  #procs .= (HashMap.insert pid Process {procStep = Li1, ..} <$> plain #procs)

  return (procStep == Li3c)

nextLi4a :: (?constants :: Constants) => ProcessLabel -> Action (VariableCtxt DijkstraMutexSpec) Bool
nextLi4a pid = do
  let Constants {..} = ?constants
  Process {..} <- (HashMap.! pid) <$> plain #procs

  #goCritical .= (HashMap.insert pid False <$> plain #goCritical)
  #procs .= (HashMap.insert pid Process {procStep = Li4b, iterState = List.delete pid processSet, ..} <$> plain #procs)

  return (procStep == Li4a)

nextLi4b ::
  (?constants :: Constants) =>
  ProcessLabel ->
  Action (VariableCtxt DijkstraMutexSpec) Bool
nextLi4b pid = do
  let Constants {..} = ?constants
  Process {..} <- (HashMap.! pid) <$> plain #procs

  if iterState /= []
    then do
      pid' <- oneOf processSet
      goCrit <- (HashMap.! pid') <$> plain #goCritical
      let iterState' = List.delete pid' iterState

      if not goCrit
        then #procs .= (HashMap.insert pid Process {procStep = Li1, iterState = iterState', ..} <$> plain #procs)
        else #procs .= (HashMap.insert pid Process {procStep = Li4b, iterState = iterState', ..} <$> plain #procs)
    else do
      #procs .= (HashMap.insert pid Process {procStep = Critical, ..} <$> plain #procs)

  return (procStep == Li4b)

nextCrit :: ProcessLabel -> Action (VariableCtxt DijkstraMutexSpec) Bool
nextCrit pid = do
  Process {..} <- (HashMap.! pid) <$> plain #procs

  #procs .= (HashMap.insert pid Process {procStep = Li5, ..} <$> plain #procs)

  return (procStep == Critical)

nextLi5 :: ProcessLabel -> Action (VariableCtxt DijkstraMutexSpec) Bool
nextLi5 pid = do
  Process {..} <- (HashMap.! pid) <$> plain #procs

  #goCritical .= (HashMap.insert pid True <$> plain #goCritical)

  return (procStep == Li5)

nextLi6 :: ProcessLabel -> Action (VariableCtxt DijkstraMutexSpec) Bool
nextLi6 pid = do
  Process {..} <- (HashMap.! pid) <$> plain #procs

  #isLooping .= (HashMap.insert pid True <$> plain #isLooping)
  #procs .= (HashMap.insert pid Process {procStep = Noncritical, ..} <$> plain #procs)

  return (procStep == Li6)

nextNoncrit :: ProcessLabel -> Action (VariableCtxt DijkstraMutexSpec) Bool
nextNoncrit pid = do
  Process {..} <- (HashMap.! pid) <$> plain #procs

  #procs .= (HashMap.insert pid Process {procStep = Li0, ..} <$> plain #procs)

  return (procStep == Noncritical)

mutualExclusion :: (?constants :: Constants) => Action (VariableCtxt DijkstraMutexSpec) Bool
mutualExclusion = do
  let Constants {..} = ?constants

  forall processSet \i ->
    forall processSet \j -> do
      procI <- (HashMap.! i) <$> plain #procs
      procJ <- (HashMap.! j) <$> plain #procs

      return (not (procStep procI == Critical && procStep procJ == Critical) || i == j)

spec :: DijkstraMutexSpec
spec =
  let Constants {..} = mkProcesses 3

      specInit =
        #isLooping := return (HashMap.fromList [(pid, True) | pid <- processSet])
          :. #goCritical := return (HashMap.fromList [(pid, True) | pid <- processSet])
          :. #procIx := return (ProcessLabel 0)
          :. #procs := return (HashMap.fromList [(pid, Process Li0 (ProcessLabel 0) processSet) | pid <- processSet])

      specNext =
        let ?constants = Constants {..}
         in WeakFairAction #next next
              :\/: WeakFairAction #mutualExclusion mutualExclusion
   in Spec specInit specNext
  where
    mkProcesses :: Int -> Constants
    mkProcesses n = Constants n (ProcessLabel <$> [0 .. n - 1])

test :: Either [MCError (VariableCtxt DijkstraMutexSpec)] MCMetrics
test = modelCheck spec

check :: IO ()
check = defaultInteraction spec
