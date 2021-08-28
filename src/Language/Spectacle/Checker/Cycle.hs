{-# LANGUAGE GeneralizedNewtypeDeriving #-}

-- |
--
-- @since 0.1.0.0
module Language.Spectacle.Checker.Cycle
  ( Cycle (Cycle, runCycle),
    CycleKind (DeadEnd, ClosedCycle),
    fromCycleKind,
    takeClosedCycles,
    takeCyclesFrom,
  )
where

import Control.Applicative
import Control.Monad
import Control.Monad.State
import Control.Monad.Writer
import Data.Function
import Data.IntMap (IntMap)
import qualified Data.IntMap as IntMap
import Data.IntSet (IntSet)
import qualified Data.IntSet as IntSet
import qualified Debug.Trace as Debug
import Data.Maybe
import Data.Kind
import Data.Coerce
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Foldable
import Lens.Micro

import Control.Monad.Levels
import Data.Bag (Bag)
import qualified Data.Bag as Bag
import Language.Spectacle.Checker.CoverageMap
import Language.Spectacle.Syntax.NonDet
import Language.Spectacle.Checker.Cover
import Language.Spectacle.Checker.Fingerprint

-- ---------------------------------------------------------------------------------------------------------------------

newtype Cycle :: Type -> Type where
  Cycle ::
    { runCycle ::
        WriterT [Fingerprint] (LevelsT (StateT (IntMap IntSet) [])) a
    } ->
    Cycle a
  deriving (Functor, Applicative, Monad, Alternative)
  deriving (MonadState (IntMap IntSet), MonadWriter [Fingerprint])

data CycleKind
  = DeadEnd Fingerprint
  | ClosedCycle Fingerprint
  deriving Show

fromCycleKind :: CycleKind -> Fingerprint
fromCycleKind (DeadEnd fingerprint) = fingerprint
fromCycleKind (ClosedCycle fingerprint) = fingerprint

takeClosedCycles :: Fingerprint -> [(CycleKind, [Fingerprint])] -> [(CycleKind, [Fingerprint])]
takeClosedCycles fingerprint = filter \case
  (ClosedCycle cycles, _) -> cycles == fingerprint
  (DeadEnd _, _) -> False

takeCyclesFrom :: Fingerprint -> CoverageMap -> [(CycleKind, [Fingerprint])]
takeCyclesFrom goal graph =
  stepCycleInitial graph goal
    & runCycle
    & runWriterT
    & runLevelsA
    & flip evalStateT IntMap.empty
    & fmap toList
    & concat

stepCycleInitial :: CoverageMap -> Fingerprint -> Cycle CycleKind
stepCycleInitial graph goal = do
  there <- oneOf (takeWorldsThere graph goal)
  cycles <- stepCycle graph goal there

  if cycles == goal
    then return (ClosedCycle there)
    else return (DeadEnd there)

stepCycle :: CoverageMap -> Fingerprint -> Fingerprint -> Cycle Fingerprint
stepCycle graph goal here
  | goal == here = do
      tell [here]
      return here
  | graph ^. (coverageInfo here . hasBeenExplored . to not) = do
      tell [here]
      return here
  | isDisabled graph here = empty
  | otherwise = do
    there <- oneOf (takeWorldsThere graph here)
    stepLocate here there
    stepCycle graph goal there

takeWorldsThere :: CoverageMap -> Fingerprint -> Set Fingerprint
takeWorldsThere graph fingerprint =
  let nexts = graph ^. (coverageInfo fingerprint . nextWorlds)
      disabled = graph ^. (coverageInfo fingerprint . disabledNextWorlds)
  in nexts `Set.difference` disabled

stepLocate :: Fingerprint -> Fingerprint -> Cycle ()
stepLocate (Fingerprint here) (Fingerprint there) = do
  guard =<< isUnique here there
  tell [Fingerprint here]
  seenNexts <- gets (IntMap.lookup here)

  case seenNexts of
    Nothing -> modify (IntMap.insert here (IntSet.singleton there))
    Just nexts -> modify (IntMap.insertWith IntSet.union here (IntSet.singleton there))

isDisabled :: CoverageMap -> Fingerprint -> Bool
isDisabled graph fingerprint =
  let nexts = graph ^. (coverageInfo fingerprint . nextWorlds)
      disabled = graph ^. (coverageInfo fingerprint . disabledNextWorlds)
  in nexts == disabled

isUnique :: Int -> Int -> Cycle Bool
isUnique here there = do
  seenNexts <- gets (IntMap.lookup here)
  return (maybe True (IntSet.notMember there) seenNexts)
