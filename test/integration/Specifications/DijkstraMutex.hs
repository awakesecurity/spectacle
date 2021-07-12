{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE RecordWildCards #-}

module Specifications.DijkstraMutex where

import Control.Applicative (Applicative (liftA2))
import Control.Monad (forM)
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap
import Data.Hashable (Hashable)
import qualified Data.List as List
import Numeric.Natural (Natural)

import Data.Type.Rec (Name, type (#), type (.|))
import Language.Spectacle
  ( Action,
    Initial,
    Invariant,
    always,
    defaultInteraction,
    define,
    eventually,
    exists,
    modelCheck,
    oneOf,
    plain,
    weakFair,
    (.=),
    (/\),
    (==>),
  )
import Language.Spectacle.RTS.Registers (RelationTerm)
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

import Specifications.DijkstraMutex.Process
  ( Process (Process, procStep, iterState, pix),
    ProcessLabel (ProcessLabel),
    StepLabel
      ( Critical,
        Li0,
        Li1,
        Li2,
        Li3a,
        Li3b,
        Li3c,
        Li4a,
        Li4b,
        Li5,
        Li6,
        Noncritical
      ),
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
  #isLooping `define` return (HashMap.fromList [(pid, True) | pid <- processSet])

  #goCritical `define` return (HashMap.fromList [(pid, True) | pid <- processSet])

  #procIx `define` return (ProcessLabel 0)

  #procs `define` do
    HashMap.fromList <$> forM processSet \pid -> do
      return (pid, Process Li0 (ProcessLabel 0) processSet)

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
      Li1 -> do
        procIx <- plain #procIx
        if procIx /= pid
          then #procs .= #procs `except` (pid, Process {procStep = Li2, ..})
          else #procs .= #procs `except` (pid, Process {procStep = Li4a, ..})
      Li2 -> do
        #goCritical .= #goCritical `except` (pid, True)
        #procs .= #procs `except` (pid, Process {procStep = Li3a, ..})
      Li3a -> do
        procIx <- plain #procIx
        #procs .= #procs `except` (pid, Process {pix = procIx, procStep = Li3b, ..})
      Li3b -> do
        looping <- (HashMap.! pix) <$> plain #isLooping
        if looping
          then #procs .= #procs `except` (pid, Process {procStep = Li3c, ..})
          else #procs .= #procs `except` (pid, Process {procStep = Li1, ..})
      Li3c -> do
        #procIx .= return pid
        #procs .= #procs `except` (pid, Process {procStep = Li1, ..})
      Li4a -> do
        #goCritical .= #goCritical `except` (pid, False)
        #procs .= #procs `except` (pid, Process {procStep = Li4b, iterState = List.delete pid processSet, ..})
      Li4b -> do
        if iterState /= []
          then do
            pid' <- oneOf processSet
            goCrit <- (HashMap.! pid') <$> plain #goCritical
            let iterState' = List.delete pid' iterState
            if not goCrit
              then #procs .= #procs `except` (pid, Process {procStep = Li1, iterState = iterState', ..})
              else #procs .= #procs `except` (pid, Process {procStep = Li4b, iterState = iterState', ..})
          else do
            #procs .= #procs `except` (pid, Process {procStep = Critical, ..})
      Critical -> do
        #procs .= #procs `except` (pid, Process {procStep = Li5, ..})
      Li5 -> do
        #goCritical .= #goCritical `except` (pid, True)
        #procs .= #procs `except` (pid, Process {procStep = Li6, ..})
      Li6 -> do
        #isLooping .= #isLooping `except` (pid, True)
        #procs .= #procs `except` (pid, Process {procStep = Noncritical, ..})
      Noncritical -> do
        #procs .= #procs `except` (pid, Process {procStep = Li0, ..})
    return True

except :: (Eq a, Hashable a, s # HashMap a b .| ctx) => Name s -> (a, b) -> RelationTerm ctx (HashMap a b)
except name (k, v) = HashMap.insert k v <$> plain name

formula :: (?constants :: Constants) => Invariant DijkstraMutexSpec Bool
formula = do
  mutualExclusion /\ starvationFree

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
  areAllFirstStep ==> eventually willGoCritical
  where
    areAllFirstStep :: (?constants :: Constants) => Invariant DijkstraMutexSpec Bool
    areAllFirstStep = do
      let Constants {..} = ?constants
          xs = flip map processSet \pid -> do
            p <- (HashMap.! pid) <$> plain #procs
            return (procStep p == Critical)
      foldr (liftA2 (&&)) (pure True) xs

    willGoCritical :: (?constants :: Constants) => Invariant DijkstraMutexSpec Bool
    willGoCritical = do
      let Constants {..} = ?constants
      and <$> forM processSet \pid -> do
        p <- (HashMap.! pid) <$> plain #procs
        return (procStep p == Critical)

check :: IO ()
check = do
  let ?constants = mkProcesses 2
  let spec :: Specification DijkstraMutexSpec
      spec =
        Specification
          { initialAction = initial
          , nextAction = next
          , temporalFormula = formula
          , terminationFormula = Nothing
          , fairnessConstraint = weakFair
          }
  defaultInteraction (modelCheck spec)
  where
    mkProcesses :: Natural -> Constants
    mkProcesses n = Constants n (ProcessLabel <$> [0 .. n - 1])
