{-# LANGUAGE RecordWildCards #-}

-- |
--
-- @since 0.1.0.0
module Language.Spectacle.Model.MCState
  ( MCState (MCState),
    _mcStateCoverage,
    mcStateCoverage,
    mcStateIsMarked,
  )
where

import Data.Set (Set)
import qualified Data.Set as Set
import Lens.Micro

import Data.World
import Language.Spectacle.Checker.Fingerprint

-- ---------------------------------------------------------------------------------------------------------------------

data MCState = MCState
  { _mcStateCoverage :: Set Fingerprint
  }
  deriving Show

mcStateCoverage :: Lens' MCState (Set Fingerprint)
mcStateCoverage = lens _mcStateCoverage \MCState {..} x -> MCState {_mcStateCoverage = Set.union _mcStateCoverage x, ..}

mcStateIsMarked :: World ctxt -> SimpleGetter MCState Bool
mcStateIsMarked (World fp _) = mcStateCoverage . to (Set.member fp)
