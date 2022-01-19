-- |
--
-- @since 0.1.0.0
module Language.Spectacle.Model.MCEnv
  ( -- * Model Environments
    MCEnv (MCEnv),
    mcEnvPropInfo,
    mcEnvUnfairActions,
    mcEnvWeakFairActions,
    mcEnvStrongFairActions,

    -- ** Lenses
    propInfo,
    unfairActions,
    weakFairActions,
    strongFairActions,
  )
where

import Data.Map.Strict (Map)
import Lens.Micro (SimpleGetter, to)

import Language.Spectacle.Specification
import Language.Spectacle.Specification.Prop

-- ---------------------------------------------------------------------------------------------------------------------

data MCEnv = MCEnv
  { mcEnvPropInfo :: Map String (Fairness, PropInfo)
  , mcEnvUnfairActions :: Set String
  , mcEnvWeakFairActions :: Set String
  , mcEnvStrongFairActions :: Set String
  }

makeMCEnv :: Map String (Fairness, PropInfo) -> MCEnv
makeMCEnv info =
  let fairnessMap = snd <$> info
   in MCEnv info _ _ _

propInfo :: SimpleGetter MCEnv (Map String (Fairness, PropInfo))
propInfo = to mcEnvPropInfo

unfairActions :: SimpleGetter MCEnv (Set String)
unfairActions = to mcEnvUnfairActions

weakFairActions :: SimpleGetter MCEnv (Set String)
weakFairActions = to mcEnvUnfairActions

strongFairActions :: SimpleGetter MCEnv (Set String)
strongFairActions = to mcEnvUnfairActions
