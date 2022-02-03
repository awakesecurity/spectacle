{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE UndecidableInstances #-}

-- |
--
-- @since 1.0.0
module Language.Spectacle.Model.ModelState
  ( -- * ModelState
    ModelState (ModelState),
    getModelState,

    -- ** Query
    member,
    enabledActionsAt,

    -- ** Lenses
    indexNode,
    queuedActionsAt,
  )
where

import Data.Coerce (coerce)
import Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict as IntMap
import Data.Set (Set)
import Lens.Micro (Lens', lens, (^.))

import Data.Fingerprint (Fingerprint (Fingerprint))
import Data.Type.Rec (HasDict)
import Language.Spectacle.Model.ModelNode (ModelNode, actionsOf, isEnabled, queuedOf)

-- ---------------------------------------------------------------------------------------------------------------------

newtype ModelState ctx = ModelState
  {getModelState :: IntMap (ModelNode ctx)}
  deriving (Semigroup, Monoid) via IntMap (ModelNode ctx)

-- | @since 1.0.0
deriving instance HasDict Show ctx => Show (ModelState ctx)

indexNode :: Fingerprint -> Lens' (ModelState ctx) (ModelNode ctx)
indexNode ~(Fingerprint hash) =
  let getter (ModelState nodes) = nodes IntMap.! hash
      setter (ModelState nodes) ns = ModelState (IntMap.insert hash ns nodes)
   in lens getter setter

queuedActionsAt :: Fingerprint -> Lens' (ModelState ctx) (Set String)
queuedActionsAt hash = indexNode hash . queuedOf

enabledActionsAt :: Fingerprint -> ModelState ctx -> [String]
enabledActionsAt hash st =
  let node = st ^. indexNode hash
      actions = node ^. actionsOf
   in filter (\action -> node ^. isEnabled action) actions

member :: Fingerprint -> ModelState ctx -> Bool
member hash = IntMap.member (coerce hash) . getModelState
