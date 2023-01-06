{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE UndecidableInstances #-}

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
module Language.Spectacle.Model.ModelNode
  ( -- * Model State Nodes
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
  )
where

import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import Lens.Micro (Lens', SimpleGetter, lens, to)

import Data.Fingerprint (Fingerprint)
import Data.Type.Rec (HasDict, Rec)

-- ---------------------------------------------------------------------------------------------------------------------

data ModelNode ctx = ModelNode
  { nodeNextEntries :: Map String [Fingerprint]
  , nodeActionQueue :: Set String
  , nodeValuation :: Rec ctx
  }

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
