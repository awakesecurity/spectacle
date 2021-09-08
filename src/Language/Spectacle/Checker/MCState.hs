{-# LANGUAGE RecordWildCards #-}

-- |
--
-- @since 0.1.0.0
module Language.Spectacle.Checker.MCState
  ( -- *
    MCState (MCState),
    _mcStateEnabledMap,
    _mcStateMaxDepth,
    _mcStateMaxWidth,

    -- **
    mcStateCoverageMap,
    mcStateIsEnabledAt,
    mcStateSetEnabled,
    mcStateMaxDepth,
    mcStateMaxWidth,
  )
where

import Data.Coerce
import Data.Foldable
import Data.Hashable
import Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict as IntMap
import Data.Set (Set)
import qualified Data.Set as Set
import Lens.Micro

import Data.World
import Data.Type.Rec
import Language.Spectacle.Checker.Fingerprint
import Language.Spectacle.Checker.MCCoverageMap (MCCoverageMap)
import qualified Language.Spectacle.Checker.MCCoverageMap as MCCoverageMap
import Language.Spectacle.Checker.MCWorldInfo

-- ---------------------------------------------------------------------------------------------------------------------

data MCState = MCState
  { _mcStateEnabledMap :: IntMap (Set (String, Bool))
  , _mcStateCoverageMap :: MCCoverageMap
  , _mcStateMaxDepth :: Int
  , _mcStateMaxWidth :: Int
  }

mcStateIsEnabledAt :: World ctxt -> String -> MCState -> Maybe Bool
mcStateIsEnabledAt (World fingerprint _) action MCState {..} = do
  enableds <- IntMap.lookup (coerce fingerprint) _mcStateEnabledMap
  enabled <- find (\(action', _) -> action' == action) enableds
  return (snd enabled)

mcStateSetEnabled :: World ctxt -> String -> Bool -> MCState -> MCState
mcStateSetEnabled (World fingerprint _) action enabled MCState {..} =
  MCState
    { _mcStateEnabledMap =
        IntMap.insertWith
          Set.union
          (coerce fingerprint)
          (Set.singleton (action, enabled))
          _mcStateEnabledMap
    , ..
    }

mcStateCoverageMap :: Lens' MCState MCCoverageMap
mcStateCoverageMap = lens _mcStateCoverageMap \MCState {..} x -> MCState {_mcStateCoverageMap = x, ..}

mcStateMaxDepth :: Lens' MCState Int
mcStateMaxDepth = lens _mcStateMaxDepth \MCState {..} x -> MCState {_mcStateMaxDepth = max _mcStateMaxDepth x, ..}

mcStateMaxWidth :: Lens' MCState Int
mcStateMaxWidth = lens _mcStateMaxWidth \MCState {..} x -> MCState {_mcStateMaxWidth = max _mcStateMaxWidth x, ..}
