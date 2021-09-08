{-# LANGUAGE RecordWildCards #-}

-- |
--
-- @since 0.1.0.0
module Language.Spectacle.Checker.MCEnv
  ( -- * Model Checker Environment
    MCEnv (MCEnv),
    _mcEnvActionInfo,
    _mcEnvActionSpine,
    _mcEnvPropInfo,
    mcEnvActionInfo,
    mcEnvActionSpine,
    mcEnvPropInfo,
  )
where

import Data.IntMap.Strict (IntMap)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import GHC.Stack (SrcLoc)
import Lens.Micro (Lens', lens)
import Data.Kind

import Language.Spectacle.Specification
import Language.Spectacle.Specification.Action
import Language.Spectacle.Specification.Prop
import Data.Context

-- ---------------------------------------------------------------------------------------------------------------------

type MCEnv :: Context -> [Type] -> Type
data MCEnv ctxt action = MCEnv
  { _mcEnvActionInfo :: Map String ActionInfo
  , _mcEnvActionSpine :: ActionSpine ctxt action
  , _mcEnvPropInfo :: Map String PropInfo
  }

mcEnvActionInfo :: Lens' (MCEnv ctxt acts) (Map String ActionInfo)
mcEnvActionInfo = lens _mcEnvActionInfo \MCEnv {..} x -> MCEnv {_mcEnvActionInfo = x, ..}
{-# INLINE mcEnvActionInfo #-}

mcEnvActionSpine :: Lens' (MCEnv ctxt actions) (ActionSpine ctxt actions)
mcEnvActionSpine = lens _mcEnvActionSpine \MCEnv {..} x -> MCEnv {_mcEnvActionSpine = x, ..}
{-# INLINE mcEnvActionSpine #-}

mcEnvPropInfo :: Lens' (MCEnv ctxt acts) (Map String PropInfo)
mcEnvPropInfo = lens _mcEnvPropInfo \MCEnv {..} x -> MCEnv {_mcEnvPropInfo = x, ..}
{-# INLINE mcEnvPropInfo #-}