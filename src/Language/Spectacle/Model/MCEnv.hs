-- |
--
-- @since 0.1.0.0
module Language.Spectacle.Model.MCEnv
  ( -- * Model Environments
    MCEnv (MCEnv),
    _propInfo,

    -- ** Lenses
    mcEnvPropInfo,
  )
where

import Data.Map.Strict (Map)
import Lens.Micro (SimpleGetter, to)

import Language.Spectacle.Specification (Fairness, PropInfo)

-- ---------------------------------------------------------------------------------------------------------------------

data MCEnv = MCEnv
  { _propInfo :: Map String (Fairness, PropInfo)
  }

mcEnvPropInfo :: SimpleGetter MCEnv (Map String (Fairness, PropInfo))
mcEnvPropInfo = to _propInfo
{-# INLINE mcEnvPropInfo #-}
