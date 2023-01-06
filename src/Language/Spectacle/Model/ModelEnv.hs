{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_HADDOCK show-extensions #-}

-- |
-- Module      :  Language.Spectacle.Model.ModelEnv
-- Copyright   :  (c) Arista Networks, 2022-2023
-- License     :  Apache License 2.0, see LICENSE
--
-- Stability   :  stable
-- Portability :  non-portable (GHC extensions)
--
-- TODO: docs
--
-- @since 1.0.0
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
    modalityOf,
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
import Lens.Micro (Lens', SimpleGetter, lens, to)

import Language.Spectacle.Fairness (Fairness (StrongFair, Unfair, WeakFair))
import Language.Spectacle.Specification (Modality)

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

modalityOf :: String -> SimpleGetter ModelEnv Modality
modalityOf name = to \env ->
  mcEnvFormulaModality env Map.! name

fairnessOf :: String -> SimpleGetter ModelEnv Fairness
fairnessOf name = to \env ->
  mcEnvActionFairness env Map.! name

actionInfo :: SimpleGetter ModelEnv (Map String Fairness)
actionInfo = to mcEnvActionFairness

unfairActions :: SimpleGetter ModelEnv (Set String)
unfairActions = to mcEnvUnfairActions

weakFairActions :: Lens' ModelEnv (Set String)
weakFairActions = lens mcEnvWeakFairActions \ModelEnv {..} actions ->
  ModelEnv {mcEnvWeakFairActions = actions, ..}

strongFairActions :: Lens' ModelEnv (Set String)
strongFairActions = lens mcEnvStrongFairActions \ModelEnv {..} actions ->
  ModelEnv {mcEnvStrongFairActions = actions, ..}
