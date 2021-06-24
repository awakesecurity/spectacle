{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE RecordWildCards #-}

module Specifications.DijkstraMutex where

import Control.Monad (forM)
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap
import Data.Hashable (Hashable)
import qualified Data.List as List
import Data.Type.Rec (Name, type (#), type (.|))
import Language.Spectacle
  ( Action,
    Initial,
    Invariant,
    Terminate,
    always,
    define,
    doModelCheck,
    eventually,
    exists,
    oneOf,
    plain,
    (.=),
    (/\),
    (==>),
    (\/),
  )
import Language.Spectacle.Exception (SpecException (ModelCheckerException, RuntimeException))
import Language.Spectacle.RTS.Registers (RelationTerm)
import Language.Spectacle.Spec.Base (Fairness (WeaklyFair))
import Numeric.Natural (Natural)

import Specifications.DijkstraMutex.Process
  ( Process (Process, iterState, pix, procStep),
    ProcessLabel (ProcessLabel),
    StepLabel (Critical, Done, Li0, Li1, Li2, Li3a, Li3b, Li3c, Li4a, Li4b, Li5, Li6, Noncritical),
  )

-- ---------------------------------------------------------------------------------------------------------------------

data Constants = Constants
  { numProcesses :: Natural
  , processSet :: [ProcessLabel]
  }

type DijkstraMutexSpec =
  '[ "isLooping" # HashMap ProcessLabel Bool
   , "goCritical" # HashMap ProcessLabel Bool
   , "procIx" # ProcessLabel
   , "procs" # HashMap ProcessLabel Process
   ]

initial :: (?constants :: Constants) => Initial DijkstraMutexSpec ()
initial = do
  let Constants {..} = ?constants
  procIx <- oneOf processSet
  startSteps <- oneOf [Li0] -- Either we start at Li0 or at Done (the process never started).
  #isLooping `define` return (HashMap.fromList [(pid, True) | pid <- processSet])

  #goCritical `define` return (HashMap.fromList [(pid, True) | pid <- processSet])

  #procIx `define` return procIx

  #procs `define` do
    HashMap.fromList <$> forM processSet \pid -> do
      return (pid, Process Li0 procIx processSet)

next :: (?constants :: Constants) => Action DijkstraMutexSpec Bool
next = nextProc

nextProc :: (?constants :: Constants) => Action DijkstraMutexSpec Bool
nextProc = do
  let Constants {..} = ?constants
  exists processSet \pid -> do
    Process {..} <- (HashMap.! pid) <$> plain #procs
    case procStep of
      Li0 -> do
        #isLooping .= #isLooping `except` (pid, False)
        #procs .= #procs `except` (pid, Process {procStep = Li1, ..})
        return True
      Li1 -> do
        procIx <- plain #procIx
        if procIx /= pid
          then #procs .= #procs `except` (pid, Process {procStep = Li2, ..})
          else #procs .= #procs `except` (pid, Process {procStep = Li4a, ..})
        return True
      Li2 -> do
        #goCritical .= #goCritical `except` (pid, True)
        #procs .= #procs `except` (pid, Process {procStep = Li3a, ..})
        return True
      Li3a -> do
        procIx <- plain #procIx
        #procs .= #procs `except` (pid, Process {pix = procIx, procStep = Li3b, ..})
        return True
      Li3b -> do
        looping <- (HashMap.! pix) <$> plain #isLooping
        if looping
          then #procs .= #procs `except` (pid, Process {procStep = Li3c, ..})
          else #procs .= #procs `except` (pid, Process {procStep = Li1, ..})
        return True
      Li3c -> do
        #procIx .= return pid
        #procs .= #procs `except` (pid, Process {procStep = Li1, ..})
        return True
      Li4a -> do
        #goCritical .= #goCritical `except` (pid, False)
        #procs .= #procs `except` (pid, Process {procStep = Li4b, iterState = List.delete pid processSet, ..})
        return True
      Li4b -> do
        if iterState /= []
          then do
            exists iterState \pid' -> do
              let iterState' = List.delete pid' iterState
              goCrit <- (HashMap.! pid') <$> plain #goCritical
              if not goCrit
                then #procs .= #procs `except` (pid, Process {procStep = Li1, iterState = iterState', ..})
                else #procs .= #procs `except` (pid, Process {procStep = Li4b, iterState = iterState', ..})
              return True
          else do
            #procs .= #procs `except` (pid, Process {procStep = Critical, ..})
            return True
      Critical -> do
        #procs .= #procs `except` (pid, Process {procStep = Li5, ..})
        return True
      Li5 -> do
        #goCritical .= #goCritical `except` (pid, True)
        #procs .= #procs `except` (pid, Process {procStep = Li6, ..})
        return True
      Li6 -> do
        #isLooping .= #isLooping `except` (pid, True)
        #procs .= #procs `except` (pid, Process {procStep = Noncritical, ..})
        return True
      Noncritical -> do
        #procs .= #procs `except` (pid, Process {procStep = Li0, ..})
        return True
      Done -> return False

except :: (Eq a, Hashable a, s # HashMap a b .| ctx) => Name s -> (a, b) -> RelationTerm ctx (HashMap a b)
except name (k, v) = HashMap.insert k v <$> plain name

invariant :: (?constants :: Constants) => Invariant DijkstraMutexSpec Bool
invariant = do
  let Constants {..} = ?constants
  mutualExclusion /\ starvationFree
  where
    mutualExclusion :: (?constants :: Constants) => Invariant DijkstraMutexSpec Bool
    mutualExclusion = always do
      let Constants {..} = ?constants
      and <$> forM processSet \i ->
        and <$> forM processSet \j -> do
          if i /= j
            then do
              p_i <- (HashMap.! i) <$> plain #procs
              p_j <- (HashMap.! j) <$> plain #procs
              return (not (procStep p_i == Critical && procStep p_j == Critical))
            else return True

starvationFree :: (?constants :: Constants) => Invariant DijkstraMutexSpec Bool
starvationFree = do
  -- Every process must eventually go critical if its started, otherwise it must never start.
  (areAllFirstStep ==> eventually willGoCritical) \/ always neverStarted
  where
    areAllFirstStep :: (?constants :: Constants) => Invariant DijkstraMutexSpec Bool
    areAllFirstStep = do
      let Constants {..} = ?constants
      and <$> forM processSet \pid -> do
        p <- (HashMap.! pid) <$> plain #procs
        return (procStep p == Li0)

    willGoCritical :: (?constants :: Constants) => Invariant DijkstraMutexSpec Bool
    willGoCritical = do
      let Constants {..} = ?constants
      or <$> forM processSet \pid -> do
        p <- (HashMap.! pid) <$> plain #procs
        return (procStep p == Critical)

    neverStarted :: (?constants :: Constants) => Invariant DijkstraMutexSpec Bool
    neverStarted = do
      let Constants {..} = ?constants
      and <$> forM processSet \pid -> do
        p <- (HashMap.! pid) <$> plain #procs
        return (procStep p == Done)

terminate :: Terminate DijkstraMutexSpec Bool
terminate = do
  procs <- plain #procs
  and <$> forM procs \Process {..} -> return (procStep == Done)

check :: IO ()
check = do
  let ?constants = mkProcesses 2
  case doModelCheck initial next invariant (Just terminate) WeaklyFair of
    (Left (ModelCheckerException ws exc), _) -> do
      putStrLn "model check failed with checker exception:"
      print ws
      print exc
    (Left (RuntimeException ws exc), _) -> do
      putStrLn "model check failed with runtime exception:"
      mapM_ print ws
      print exc
    (Right _, _) -> do
      putStrLn "model success, final state: ..."
  where
    mkProcesses :: Natural -> Constants
    mkProcesses n = Constants n (ProcessLabel <$> [0 .. n - 1])
