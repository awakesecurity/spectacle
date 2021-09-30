{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedLabels #-}

-- |
--
-- === Reference
--
-- 1. "Towards a Common Categorical Semantics Linear-Time Temporal Logic Functional Reactive Programming",
--   Wolfgang Jeltsch
--
-- @since 0.1.0.0
module Control.Comonad.Global where

import Control.Applicative (Applicative (liftA2))
import Control.Monad.Except (MonadError (throwError))
import Control.Monad.State.Strict (MonadState)
import Data.Set (Set)
import qualified Data.Set as Set
import Lens.Micro.Mtl (use, view, (%=))

import Control.Temporal (Prefix (Prefix))
import Data.Temporal (Interval (Interval), Time (Inf, Time), boundryElem)
import Data.World (World, worldFingerprint)
import Language.Spectacle.Checker.MCError (MCError (MCAlwaysError))
import Language.Spectacle.Model.MCState (MCState, mcStateCoverage, mcStateIsMarked)

-- --------------------------------------------------------------------------------------------------------------------

globalNT ::
  (MonadState MCState m, MonadError [MCError ctxt] m, Show i) =>
  Prefix i (Set (World ctxt)) ->
  m [Interval i (World ctxt) (Set (World ctxt))]
globalNT (Prefix rel Inf _) = return [Interval rel Inf Inf]
globalNT (Prefix rel (Time here) there)
  | here == there = return (foldr ((:) . boundryElem rel) [] here)
  | otherwise = foldr (liftA2 (++) . globalInterval rel there) (pure []) here

globalInterval ::
  (MonadState MCState m, MonadError [MCError ctxt] m, Show i) =>
  i ->
  Set (World ctxt) ->
  World ctxt ->
  m [Interval i (World ctxt) (Set (World ctxt))]
globalInterval rel there here
  | Set.null there = throwError [MCAlwaysError here (Set.singleton (show rel))]
  | otherwise = do
    isMarked <- use (mcStateIsMarked here)
    if isMarked
      then return []
      else do
        mcStateCoverage %= Set.insert (view worldFingerprint here)
        return [Interval rel (Time here) (Time there)]
