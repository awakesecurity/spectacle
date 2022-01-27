-- |
--
-- @since 0.1.0.0
module Language.Spectacle.Model.ModelEnv
  ( -- * Model Environments
    ModelEnv (ModelEnv),
    mcEnvActionFairness,
    mcEnvFormulaModality,
    mcEnvUnfairActions,
    mcEnvWeakFairActions,
    mcEnvStrongFairActions,

    -- ** Construction
    newModelEnv,

    -- ** Lenses
    actionInfo,
    fairnessOf,
    unfairActions,
    weakFairActions,
    strongFairActions,
  )
where

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Lens.Micro (SimpleGetter, to)

import Language.Spectacle.Specification
import Language.Spectacle.Specification.Prop
import Language.Spectacle.Fairness

-- ---------------------------------------------------------------------------------------------------------------------

data ModelEnv = ModelEnv
  { mcEnvActionFairness :: Map String Fairness
  , mcEnvFormulaModality :: Map String Modality
  , mcEnvUnfairActions :: Set String
  , mcEnvWeakFairActions :: Set String
  , mcEnvStrongFairActions :: Set String
  }

newModelEnv :: Map String Fairness -> Map String Modality -> ModelEnv
newModelEnv fairInfo modalInfo =
  let unfair = Set.fromList . Map.keys $ Map.filter (Unfair ==) fairInfo
      weakfair = Set.fromList . Map.keys $ Map.filter (WeakFair ==) fairInfo
      strongfair = Set.fromList . Map.keys $ Map.filter (StrongFair ==) fairInfo
   in ModelEnv fairInfo modalInfo unfair weakfair strongfair

fairnessOf :: String -> SimpleGetter ModelEnv Fairness
fairnessOf name = to \env ->
  mcEnvActionFairness env Map.! name

actionInfo :: SimpleGetter ModelEnv (Map String Fairness)
actionInfo = to mcEnvActionFairness

unfairActions :: SimpleGetter ModelEnv (Set String)
unfairActions = to mcEnvUnfairActions

weakFairActions :: SimpleGetter ModelEnv (Set String)
weakFairActions = to mcEnvUnfairActions

strongFairActions :: SimpleGetter ModelEnv (Set String)
strongFairActions = to mcEnvUnfairActions
