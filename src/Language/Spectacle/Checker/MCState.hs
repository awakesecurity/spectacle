{-# LANGUAGE RecordWildCards #-}

-- | Model checker state.
--
-- @since 0.1.0.0
module Language.Spectacle.Checker.MCState
  ( -- * MCState
    MCState (MCState),
    _mcStateEnabledMap,
    _mcStateMaxDepth,
    _mcStateMaxWidth,

    -- ** Lenses
    mcStateCoverageMap,
    mcStateIsEnabledAt,
    mcStateSetEnabled,
    mcStateMaxDepth,
    mcStateMaxWidth,
  )
where

import Data.Foldable (find)
import Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict as IntMap
import Data.Set (Set)
import qualified Data.Set as Set
import Lens.Micro (Lens', lens)

import Data.World (World (World))
import Language.Spectacle.Checker.MCCoverageMap (MCCoverageMap)

-- ---------------------------------------------------------------------------------------------------------------------

data MCState = MCState
  { _mcStateEnabledMap :: IntMap (Set (String, Bool))
  , _mcStateCoverageMap :: MCCoverageMap
  , _mcStateMaxDepth :: Int
  , _mcStateMaxWidth :: Int
  }

mcStateIsEnabledAt :: World ctxt -> String -> MCState -> Maybe Bool
mcStateIsEnabledAt (World fingerprint _) action MCState {..} = do
  enableds <- IntMap.lookup (fromIntegral fingerprint) _mcStateEnabledMap
  enabled <- find (\(action', _) -> action' == action) enableds
  return (snd enabled)
{-# INLINE mcStateIsEnabledAt #-}

mcStateSetEnabled :: World ctxt -> String -> Bool -> MCState -> MCState
mcStateSetEnabled (World fingerprint _) action enabled MCState {..} =
  MCState
    { _mcStateEnabledMap =
        IntMap.insertWith
          Set.union
          (fromIntegral fingerprint)
          (Set.singleton (action, enabled))
          _mcStateEnabledMap
    , ..
    }
{-# INLINE mcStateSetEnabled #-}

mcStateCoverageMap :: Lens' MCState MCCoverageMap
mcStateCoverageMap = lens _mcStateCoverageMap \MCState {..} x -> MCState {_mcStateCoverageMap = x, ..}
{-# INLINE mcStateCoverageMap #-}

mcStateMaxDepth :: Lens' MCState Int
mcStateMaxDepth = lens _mcStateMaxDepth \MCState {..} x -> MCState {_mcStateMaxDepth = max _mcStateMaxDepth x, ..}
{-# INLINE mcStateMaxDepth #-}

mcStateMaxWidth :: Lens' MCState Int
mcStateMaxWidth = lens _mcStateMaxWidth \MCState {..} x -> MCState {_mcStateMaxWidth = max _mcStateMaxWidth x, ..}
{-# INLINE mcStateMaxWidth #-}
