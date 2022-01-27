{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

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

import Data.Function
import Data.Hashable (Hashable)
import Data.Kind (Constraint, Type)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set
import GHC.TypeLits

import Data.Type.Rec as Rec
import Data.World
import Language.Spectacle.AST.Action
import Language.Spectacle.AST.Temporal
import Language.Spectacle.Fairness
import Language.Spectacle.Lang (Lang, runLang)
import Language.Spectacle.Specification.Action
import Language.Spectacle.Specification.Prop
import Language.Spectacle.Specification.Variable
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

runInitialSpec :: HasDict Hashable ctx => Specification ctx acts form -> Set (World ctx)
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
