{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_HADDOCK show-extensions #-}

-- |
-- Module      :  Language.Spectacle.Model.ModelState
-- Copyright   :  (c) Arista Networks, 2022-2023
-- License     :  Apache License 2.0, see LICENSE
--
-- Stability   :  stable
-- Portability :  non-portable (GHC extensions)
--
-- TODO: docs
--
-- @since 1.0.0
module Language.Spectacle.Model.ModelState (
  -- * ModelState
  ModelState (ModelState),
  model_state,

  -- ** Query
  member,
  enabledActionsAt,

  -- ** Lenses
  indexNode,
  queuedActionsAt,
  evidence,
  indexEvidence,
  insertEvidence,
) where

import Data.Coerce (coerce)
import Data.IntMap.Strict (IntMap)
import Data.IntMap.Strict qualified as IntMap
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Set (Set)
import Lens.Micro (Lens', lens, (^.))

import Data.Fingerprint (Fingerprint (Fingerprint))
import Data.Maybe (fromMaybe)
import Data.Type.Rec (HasDict)
import Language.Spectacle.Model.ModelNode (ModelNode, actionsOf, isEnabled, queuedOf)

-- ---------------------------------------------------------------------------------------------------------------------

data ModelState ctx = ModelState
  { model_state :: IntMap (ModelNode ctx)
  , model_evidence :: IntMap (Map String (Map String Bool))
  -- ^ TODO: docs (FingerPrint, ActionName, FormulaName) -> Bool
  }

-- | @since 1.0.0
instance HasDict Eq ctx => Semigroup (ModelState ctx) where
  state1 <> state2 =
    ModelState
      { model_state = IntMap.unionWith (<>) (model_state state1) (model_state state2)
      , model_evidence = IntMap.unionWith (Map.unionWith (Map.unionWith (||))) (model_evidence state1) (model_evidence state2)
      }
  {-# INLINE (<>) #-}

-- | @since 1.0.0
instance HasDict Eq ctx => Monoid (ModelState ctx) where
  mempty = ModelState IntMap.empty IntMap.empty
  {-# INLINE CONLIKE mempty #-}

-- | @since 1.0.0
deriving instance HasDict Show ctx => Show (ModelState ctx)

indexNode :: Fingerprint -> Lens' (ModelState ctx) (ModelNode ctx)
indexNode hash =
  let getter state = model_state state IntMap.! coerce hash
      setter state ns = ModelState (IntMap.insert (coerce hash) ns (model_state state)) (model_evidence state)
   in lens getter setter

queuedActionsAt :: Fingerprint -> Lens' (ModelState ctx) (Set String)
queuedActionsAt hash = indexNode hash . queuedOf

enabledActionsAt :: Fingerprint -> ModelState ctx -> [String]
enabledActionsAt hash st =
  let node = st ^. indexNode hash
      actions = node ^. actionsOf
   in filter (\action -> node ^. isEnabled action) actions

member :: Fingerprint -> ModelState ctx -> Bool
member hash = IntMap.member (coerce hash) . model_state

evidence ::
  Fingerprint ->
  String ->
  String ->
  Lens' (ModelState ctx) Bool
evidence hash action formula =
  lens
    (indexEvidence hash action formula)
    (flip (insertEvidence hash action formula))

-- | TODO: docs
indexEvidence ::
  -- | A fingerprint of the model checker state.
  Fingerprint ->
  -- | A string with the name of a model's action.
  String ->
  -- | A string with the name of a model's temporal formula.
  String ->
  -- | The model checker state to query.
  ModelState ctx ->
  -- | A 'Bool' value that is 'True; in the case that the triple:
  --
  -- @
  -- 'indexEvidence' (hash, action, formula)
  -- @
  --
  -- has evidence that proceeding from the world @hash@ with action @action@
  -- satisfies the temporal property @formula@.
  Bool
indexEvidence hash action formula state =
  fromMaybe False do
    xs <- IntMap.lookup (coerce hash) (model_evidence state)
    ys <- Map.lookup action xs
    Map.lookup formula ys

-- | TODO: docs
insertEvidence ::
  -- | A fingerprint of the model checker state.
  Fingerprint ->
  -- | A string with the name of a model's action.
  String ->
  -- | A string with the name of a model's temporal formula.
  String ->
  -- | TODO: docs
  Bool ->
  -- | The model checker state to query.
  ModelState ctx ->
  -- | TODO: docs
  ModelState ctx
insertEvidence hash action formula sat state =
  case IntMap.lookup (coerce hash) (model_evidence state) of
    Nothing ->
      state
        { model_evidence =
            IntMap.insert
              (coerce hash)
              (Map.singleton action (Map.singleton formula sat))
              (model_evidence state)
        }
    Just xs -> case Map.lookup action xs of
      Nothing ->
        state
          { model_evidence =
              IntMap.insert
                (coerce hash)
                (Map.insert action (Map.singleton formula sat) xs)
                (model_evidence state)
          }
      Just ys -> case Map.lookup formula ys of
        Nothing ->
          state
            { model_evidence =
                IntMap.insert
                  (coerce hash)
                  (Map.insert action (Map.insert formula sat ys) xs)
                  (model_evidence state)
            }
        Just {} ->
          state
            { model_evidence =
                IntMap.insert
                  (coerce hash)
                  (Map.insert action (Map.insertWith (||) formula sat ys) xs)
                  (model_evidence state)
            }