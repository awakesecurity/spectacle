{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE RecordWildCards #-}

module Language.Spectacle.Checker.PostCheck
  ( SearchEnv (SearchEnv, _stateGraph, _searchedWorlds, _neededProperties),
    propTable,
    strongLivenessCheck,
  )
where

import Control.Monad.Except
  ( ExceptT,
    MonadError (throwError),
    join,
    runExceptT,
    unless,
  )
import Control.Monad.State.Strict (State, evalState)
import Data.Coerce (coerce)
import qualified Data.IntMap as IntMap
import Data.IntSet (IntSet)
import qualified Data.IntSet as IntSet
import Lens.Micro (Lens', SimpleGetter, lens, to, (&))
import Lens.Micro.Mtl (use, (%=), (.=))

import Language.Spectacle.Checker.Cover (livenessProperties, nextWorlds)
import Language.Spectacle.Checker.CoverageMap (CoverageMap, coverageInfo)
import Language.Spectacle.Checker.Fingerprint (Fingerprint (Fingerprint))
import Language.Spectacle.Checker.Model.MCError (MCError (MCStrongLivenessError))
import Language.Spectacle.Checker.Model.ModelEnv (ModelEnv (ModelEnv, _livenessPropertyNames, _srcLocMap))
import Language.Spectacle.Checker.Truth (TruthMap)
import Language.Spectacle.Syntax.NonDet (foldMapA)

-- ---------------------------------------------------------------------------------------------------------------------

data SearchEnv = SearchEnv
  { _stateGraph :: CoverageMap
  , _propTable :: TruthMap
  , _searchedWorlds :: IntSet
  , _neededProperties :: IntSet
  }

stateGraph :: SimpleGetter SearchEnv CoverageMap
stateGraph = to _stateGraph
{-# INLINE stateGraph #-}

propTable :: SimpleGetter SearchEnv TruthMap
propTable = to _propTable
{-# INLINE propTable #-}

searchedWorlds :: Lens' SearchEnv IntSet
searchedWorlds = lens _searchedWorlds \SearchEnv {..} x -> SearchEnv {_searchedWorlds = x, ..}
{-# INLINE searchedWorlds #-}

neededProperties :: Lens' SearchEnv IntSet
neededProperties = lens _neededProperties \SearchEnv {..} x -> SearchEnv {_neededProperties = x, ..}
{-# INLINE neededProperties #-}

strongLivenessCheck ::
  MonadError [MCError ctx] m =>
  Fingerprint ->
  CoverageMap ->
  TruthMap ->
  ModelEnv ctx ->
  m ()
strongLivenessCheck fingerprint coverage truthMap ModelEnv {..} =
  let checkResult =
        searchStrongLiveness fingerprint
          & runExceptT
          & flip evalState (SearchEnv coverage truthMap IntSet.empty _livenessPropertyNames)
   in case checkResult of
        Left unsatNames ->
          throwError (map (MCStrongLivenessError . join . (`IntMap.lookup` _srcLocMap)) (IntSet.toList unsatNames))
        Right _ -> return ()

searchStrongLiveness :: Fingerprint -> ExceptT IntSet (State SearchEnv) ()
searchStrongLiveness fingerprint = do
  searched <- IntSet.member (coerce fingerprint) <$> use searchedWorlds
  if searched
    then throwError =<< use neededProperties
    else do
      searchedWorlds %= IntSet.insert (coerce fingerprint)

      needs <- use neededProperties
      satHere <- use (stateGraph . coverageInfo fingerprint . livenessProperties)
      let needs' = IntSet.difference needs satHere

      unless (IntSet.null needs') do
        neededProperties .= needs'
        nextStates <- use (stateGraph . coverageInfo fingerprint . nextWorlds)
        foldMapA searchStrongLiveness nextStates
