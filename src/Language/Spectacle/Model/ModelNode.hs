{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_HADDOCK show-extensions #-}

-- |
-- Module      :  Language.Spectacle.Model.ModelNode
-- Copyright   :  (c) Arista Networks, 2022-2023
-- License     :  Apache License 2.0, see LICENSE
--
-- Stability   :  stable
-- Portability :  non-portable (GHC extensions)
--
-- TODO: docs
--
-- @since 1.0.0
module Language.Spectacle.Model.ModelNode (
  -- * Model State Nodes
  ModelNode (ModelNode),
  nodeNextEntries,
  nodeValuation,

  -- ** Lenses
  nextEntries,
  queuedOf,
  isEnabled,
  isDisabled,
  actionsOf,
  valuation,
) where

import Control.Exception (assert)
import Data.Fingerprint (Fingerprint)
import Data.List qualified as List
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Type.Rec (HasDict, Rec)
import Lens.Micro (Lens', SimpleGetter, lens, to)

-- ---------------------------------------------------------------------------------------------------------------------

data ModelNode ctx = ModelNode
  { nodeNextEntries :: Map String [Fingerprint]
  , nodeActionQueue :: Set String
  , nodeValuation :: Rec ctx
  }

-- | @since 1.0.0
instance HasDict Eq ctx => Semigroup (ModelNode ctx) where
  node1 <> node2 =
    ModelNode
      { nodeNextEntries =
          Map.unionWith
            List.union
            (nodeNextEntries node1)
            (nodeNextEntries node2)
      , nodeActionQueue =
          Set.intersection
            (nodeActionQueue node1)
            (nodeActionQueue node2)
      , nodeValuation =
          -- "ModelNode.(<>): valuations are not equal!" 
          assert 
            (nodeValuation node1 == nodeValuation node2)
            (nodeValuation node1) 
      }
  {-# INLINE (<>) #-}

-- | @since 1.0.0
deriving instance HasDict Show ctx => Show (ModelNode ctx)

-- | @'nextEntries' name@ produces a list of 'Fingerprint's following the action named @name@ for the node's
-- valuation.
--
-- @since 1.0.0
nextEntries :: String -> Lens' (ModelNode ctx) [Fingerprint]
nextEntries name =
  let getter ModelNode {..} = nodeNextEntries Map.! name
      setter ModelNode {..} xs =
        ModelNode {nodeNextEntries = Map.insert name xs nodeNextEntries, ..}
   in lens getter setter

queuedOf :: Lens' (ModelNode ctx) (Set String)
queuedOf =
  let getter ModelNode {..} = nodeActionQueue
      setter ModelNode {..} q = ModelNode {nodeActionQueue = q, ..}
   in lens getter setter

-- | Is the action with given name enabled?
--
-- @since 1.0.0
isEnabled :: String -> SimpleGetter (ModelNode ctx) Bool
isEnabled name = nextEntries name . to (not . null)

-- | Is the action with given name disabled?
--
-- @since 1.0.0
isDisabled :: String -> SimpleGetter (ModelNode ctx) Bool
isDisabled name = nextEntries name . to null

-- | 'actionsOf' is a lens focusing on the set of actions taken at a 'ModelNode'.
--
-- @since 1.0.0
actionsOf :: SimpleGetter (ModelNode ctx) [String]
actionsOf = to (Map.keys . nodeNextEntries)

-- | 'nodeValuation' is a lens focusing on the valuation of variables this node represents.
--
-- @since 1.0.0
valuation :: SimpleGetter (ModelNode ctx) (Rec ctx)
valuation = to nodeValuation
