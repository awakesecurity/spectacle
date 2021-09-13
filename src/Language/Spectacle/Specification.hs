{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Language.Spectacle.Specification
  ( -- *
    Spec (Spec),
    Specification,
    specInitialWorlds,

    -- ** Initial Actions
    Var ((:=)),
    type (:.) ((:.)),

    -- *** Meta Information
    HasVariables,
    type VariableCtxt,
    takeInitialActions,

    -- ** Actions
    type (!>) (UnfairAction, WeakFairAction, StrongFairAction),
    type (\/) ((:\/:)),

    -- *** Meta Information
    Fairness (Unfair, WeakFair, StrongFair),
    ReflectFair,
    reflectFair,
    ActionInfo (ActionInfo),
    actionInfoFairness,
    ActionSet (ActionSet),
    actionSetName,
    actionSetWorlds,
    ActionSpine (ActionSpineNil, ActionSpineCon),
    spineToActionInfo,
    spineToActionSets,
    HasActions,
    takeActionSpine,
    type ActionCtxt,

    -- ** Temporal Formula
    type (/\),
    Always,
    Eventually,
    type (~~>),

    -- *** Meta Information
    HasProp,
    collectPropInfo,
    PropInfo (PropInfo),
    propInfoIsAlways,
    propInfoIsEventually,
    propInfoLeadsTo,
    propInfoIsInfinitelyOften,
    propInfoIsStaysAs,
    makePropInfoAlways,
    makePropInfoEventually,
    makePropInfoLeadsTo,
    makePropInfoInfinitelyOften,
    makePropInfoStaysAs,
  )
where

import Data.Function ((&))
import Data.Hashable (Hashable)
import Data.Kind (Constraint, Type)
import Data.Set (Set)
import qualified Data.Set as Set

import Data.Context (Context)
import Data.Type.Rec (Rec, RecT (RConT, RNilT), fieldMap, pattern RCon, pattern RNil)
import Data.World (World, makeWorld)
import Language.Spectacle.Lang (runLang)
import Language.Spectacle.Specification.Action
  ( ActionInfo (ActionInfo),
    ActionSet (ActionSet, actionSetName, actionSetWorlds),
    ActionSpine (ActionSpineCon, ActionSpineNil),
    Fairness (StrongFair, Unfair, WeakFair),
    HasActions (ActionCtxt, takeActionSpine),
    ReflectFair (reflectFair),
    actionInfoFairness,
    spineToActionInfo,
    spineToActionSets,
    type (!>) (StrongFairAction, UnfairAction, WeakFairAction),
    type (\/) ((:\/:)),
  )
import Language.Spectacle.Specification.Prop
  ( Always,
    Eventually,
    HasProp (collectPropInfo),
    PropInfo
      ( PropInfo,
        propInfoIsAlways,
        propInfoIsEventually,
        propInfoIsInfinitelyOften,
        propInfoLeadsTo,
        propInfoIsStaysAs
      ),
    makePropInfoAlways,
    makePropInfoEventually,
    makePropInfoInfinitelyOften,
    makePropInfoLeadsTo,
    makePropInfoStaysAs,
    type (/\),
    type (~~>),
  )
import Language.Spectacle.Specification.Variable
  ( HasVariables (VariableCtxt, takeInitialActions),
    Var ((:=)),
    type (:.) ((:.)),
  )
import Language.Spectacle.Syntax.NonDet (runNonDetA)

-- ---------------------------------------------------------------------------------------------------------------------

type Spec :: Type -> (Context -> Type) -> (Context -> Type -> Type) -> Type
data Spec ctxt spec prop where
  Spec ::
    ctxt ->
    spec (VariableCtxt ctxt) ->
    Spec ctxt spec prop

type Specification :: Type -> (Context -> Type) -> (Context -> Type -> Type) -> Constraint
type Specification ctxt spec prop =
  ( HasVariables ctxt
  , HasActions (VariableCtxt ctxt) spec
  , HasProp prop
  )

-- | @since 0.1.0.0
instance HasVariables ctxt => HasVariables (Spec ctxt actions prop) where
  type VariableCtxt (Spec ctxt actions prop) = VariableCtxt ctxt

  takeInitialActions (Spec ctxt _) = takeInitialActions ctxt
  {-# INLINE takeInitialActions #-}

specInitialWorlds ::
  forall vars ctxt spec prop.
  (HasVariables vars, VariableCtxt vars ~ ctxt, Hashable (Rec ctxt)) =>
  Spec vars spec prop ->
  Set (World ctxt)
specInitialWorlds (Spec ctxt _) =
  takeInitialActions ctxt
    & fieldMap (runLang . runNonDetA @[])
    & seqRec
    & foldMap (Set.singleton . makeWorld)
  where
    seqRec :: RecT [] ctxt' -> [Rec ctxt']
    seqRec = \case
      RNilT -> [RNil]
      RConT name states xs -> do
        var <- states
        vars <- seqRec xs
        return (RCon name var vars)
