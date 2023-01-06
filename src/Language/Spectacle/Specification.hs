{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_HADDOCK show-extensions #-}

-- |
-- Module      :  Language.Spectacle.Specification
-- Copyright   :  (c) Arista Networks, 2022-2023
-- License     :  Apache License 2.0, see LICENSE
--
-- Stability   :  stable
-- Portability :  non-portable (GHC extensions)
--
-- TODO: docs
--
-- @since 1.0.0
module Language.Spectacle.Specification
  ( -- * Specification Constraint
    Specification (Specification),
    specInit,
    specNext,
    specProp,

    -- ** Lowering
    runInitialSpec,
    getActionFormulae,
    getTemporalFormulae,
    getFairnessSpec,
    getModalitySpec,

    -- * Re-export
    module Language.Spectacle.Specification.Action,
    module Language.Spectacle.Specification.Prop,
    module Language.Spectacle.Specification.Variable,
  )
where

import Data.Function ((&))
import Data.Hashable (Hashable)
import Data.Kind (Type)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set
import GHC.TypeLits (Symbol)

import Data.Type.Rec as Rec
  ( Ascribe,
    HasDict,
    RecF,
    foldMapF,
    mapF,
    sequenceF,
  )
import Data.World (World, makeWorld)
import Language.Spectacle.AST.Action (Action)
import Language.Spectacle.AST.Temporal (Temporal)
import Language.Spectacle.Fairness (Fairness)
import Language.Spectacle.Lang (Lang, runLang)
import Language.Spectacle.Specification.Action
  ( ActionType (ActionSF, ActionUF, ActionWF),
    toAction,
    toFairness,
  )
import Language.Spectacle.Specification.Prop
  ( Modality (Always, Eventually, Infinitely, Stays),
    TemporalType (PropF, PropFG, PropG, PropGF),
    toFormula,
    toModality,
  )
import Language.Spectacle.Specification.Variable
  ( HasVars (runInitActions),
    runInitStates,
  )
import Language.Spectacle.Syntax.NonDet (NonDet, runNonDetA)

-- ---------------------------------------------------------------------------------------------------------------------

type Specification ::
  [Ascribe Symbol Type] ->
  [Ascribe Symbol Fairness] ->
  [Ascribe Symbol Modality] ->
  Type
data Specification ctx acts form where
  Specification ::
    { specInit :: RecF (Lang '[] '[NonDet]) ctx
    , specNext :: RecF (ActionType ctx) acts
    , specProp :: RecF (TemporalType ctx) form
    } ->
    Specification ctx acts form

runInitialSpec :: (HasDict Eq ctx, HasDict Hashable ctx) => Specification ctx acts form -> Set (World ctx)
runInitialSpec spec =
  specInit spec
    & Rec.mapF (\_ -> runLang . runNonDetA @[])
    & Rec.sequenceF
    & foldMap (Set.singleton . makeWorld)

getActionFormulae :: Specification ctx acts form -> Map String (Action ctx Bool)
getActionFormulae = Rec.foldMapF (\nm act -> Map.singleton (show nm) (toAction act)) . specNext

getTemporalFormulae :: Specification ctx acts form -> Map String (Temporal ctx Bool)
getTemporalFormulae = Rec.foldMapF (\nm form -> Map.singleton (show nm) (toFormula form)) . specProp

getFairnessSpec :: Specification ctx acts form -> Map String Fairness
getFairnessSpec = Rec.foldMapF (\nm act -> Map.singleton (show nm) (toFairness act)) . specNext

getModalitySpec :: Specification ctx acts form -> Map String Modality
getModalitySpec = Rec.foldMapF (\nm form -> Map.singleton (show nm) (toModality form)) . specProp
