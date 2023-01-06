{-# OPTIONS_HADDOCK show-extensions #-}

-- |
-- Module      :  Language.Spectacle.AST.Action
-- Copyright   :  (c) Arista Networks, 2022-2023
-- License     :  Apache License 2.0, see LICENSE
--
-- Stability   :  stable
-- Portability :  non-portable (GHC extensions)
--
-- TODO: docs
--
-- @since 1.0.0
module Language.Spectacle.AST.Action
  ( -- * Temporal Actions
    type Action,
    type ActionSyntax,

    -- ** Interpreters
    runAction,
    runExceptionalAction,
    rewriteLogic,
    applyComplement,
    introduceEnv,
  )
where

import Data.Either (fromRight)
import Data.Function ((&))
import Data.Hashable (Hashable)
import Data.Kind (Type)
import Data.Set (Set)
import qualified Data.Set as Set
import GHC.TypeLits (Symbol)

import Data.Functor.Loom (hoist, runLoom, (~>~))
import Data.Type.Rec (Ascribe, Rec)
import Data.World (World, makeWorld)
import Language.Spectacle.Exception.RuntimeException
  ( RuntimeException,
  )
import Language.Spectacle.Lang
  ( EffectK,
    Lang (Op, Pure, Scoped),
    Member (projectS),
    Members,
    Op (OHere, OThere),
    Scoped (SHere, SThere),
    runLang,
  )
import Language.Spectacle.RTS.Registers (RuntimeState, emptyRuntimeState, newValues)
import Language.Spectacle.Syntax.Closure
  ( Closure,
    runActionClosure,
  )
import Language.Spectacle.Syntax.Env (Env, runEnv)
import Language.Spectacle.Syntax.Error (Error, runError)
import Language.Spectacle.Syntax.Logic
  ( Effect (Complement, Conjunct, Disjunct),
    Logic,
    complement,
    conjunct,
    disjunct,
    runLogic,
  )
import Language.Spectacle.Syntax.NonDet (NonDet, runNonDetA)
import Language.Spectacle.Syntax.Plain (Plain, runPlain)
import Language.Spectacle.Syntax.Quantifier
  ( Effect (Exists, Forall),
    Quantifier,
    exists,
    forall,
    runExceptionalQuantifier,
    runQuantifier,
  )

-- ---------------------------------------------------------------------------------------------------------------------

type Action :: [Ascribe Symbol Type] -> Type -> Type
type Action ctx = Lang ctx ActionSyntax

type ActionSyntax :: [EffectK]
type ActionSyntax =
  -- NOTE: 'Closure' must be handled before 'Quantifier'. If 'Quantifier' discharged before 'Closure', erroneous values
  -- are produced from any 'Closure' nested within a forall/exists.
  '[ Logic
   , Closure
   , Quantifier
   , Plain
   , NonDet
   , Error RuntimeException
   ]

-- | Completely evaluate a temporal action yielding either a 'RuntimeException' or a collection of new worlds accessible
-- by the action given.
--
-- @since 1.0.0
runExceptionalAction ::
  forall ctxt.
  Hashable (Rec ctxt) =>
  Rec ctxt ->
  Action ctxt Bool ->
  Either RuntimeException (Set (World ctxt))
runExceptionalAction knowns action = do
  states <-
    action
      & introduceEnv
      & rewriteLogic
      & runLogic
      & runActionClosure
      & runExceptionalQuantifier
      & runEnv (emptyRuntimeState knowns)
      & runPlain knowns
      & runNonDetA
      & runError
      & runLang

  return (takeRelatedSet states)
  where
    takeRelatedSet :: [(RuntimeState ctxt, Bool)] -> Set (World ctxt)
    takeRelatedSet = foldMap \(rst, rel) ->
      if rel
        then Set.singleton (makeWorld (newValues rst))
        else Set.empty
{-# INLINE runExceptionalAction #-}

runAction ::
  forall ctxt.
  Hashable (Rec ctxt) =>
  Rec ctxt ->
  Action ctxt Bool ->
  Set (World ctxt)
runAction knowns action =
  let states =
        action
          & introduceEnv
          & rewriteLogic
          & runLogic
          & runActionClosure
          & runQuantifier
          & runEnv (emptyRuntimeState knowns)
          & runPlain knowns
          & runNonDetA
          & runError
          & runLang
          & fromRight []
   in takeRelatedSet states
  where
    takeRelatedSet :: [(RuntimeState ctxt, Bool)] -> Set (World ctxt)
    takeRelatedSet = foldMap \(rst, rel) ->
      if rel
        then Set.singleton (makeWorld (newValues rst))
        else Set.empty
{-# INLINE runAction #-}

-- | Traverses the effects in an action, rewriting all logical operators and quantifiers scoped within a negation.
--
-- @since 1.0.0
rewriteLogic :: Members '[Logic, Quantifier, NonDet] effs => Lang ctx effs Bool -> Lang ctx effs Bool
rewriteLogic = \case
  Pure x -> pure x
  Op op k -> Op op (rewriteLogic . k)
  Scoped scoped loom -> case projectS scoped of
    Nothing -> Scoped scoped loom'
    Just eff
      | Complement m <- eff -> runLoom (loom ~>~ hoist applyComplement ~>~ hoist rewriteLogic) m
      | otherwise -> Scoped scoped loom'
    where
      loom' = loom ~>~ hoist rewriteLogic
{-# INLINE rewriteLogic #-}

-- | Reduces logical negation by applying the usual rewrite rules to quantifiers and other logical operators scoped
-- within the negation.
--
-- @since 1.0.0
applyComplement :: Members '[Logic, Quantifier, NonDet] effs => Lang ctx effs Bool -> Lang ctx effs Bool
applyComplement = \case
  Pure x -> pure x
  Op op k -> Op op (applyComplement . k)
  Scoped scoped loom -> case projectS scoped of
    Nothing -> case projectS scoped of
      Nothing -> Scoped scoped loom'
      -- ¬ (∀ x : A → p x) ≡ ∃ x : A → ¬ (p x)
      Just (Forall xs p) -> exists xs (fmap not . runLoom loom' . p)
      -- ¬ (∃ x : A → p x) ≡ ∀ x : A → ¬ (p x)
      Just (Exists xs p) -> forall xs (fmap not . runLoom loom' . p)
    -- ¬ ¬ p ≡ p
    Just (Complement m) -> runLoom loom' (fmap not m)
    -- ¬ (p ∧ q) ≡ ¬ p ∨ ¬ q
    Just (Conjunct lhs rhs) ->
      let lhs' = runLoom loom' lhs
          rhs' = runLoom loom' rhs
       in disjunct (complement lhs') (complement rhs')
    -- ¬ (p ∨ q) ≡ ¬ p ∧ ¬ q
    Just (Disjunct lhs rhs) ->
      let lhs' = runLoom loom' lhs
          rhs' = runLoom loom' rhs
       in conjunct (complement lhs') (complement rhs')
    where
      loom' = loom ~>~ hoist applyComplement
{-# INLINE applyComplement #-}

-- | Introduces the variable environment to an 'Action' "underneath" the 'Closure' effect.
--
-- @since 1.0.0
introduceEnv ::
  Lang ctx (Logic ': Closure ': Quantifier ': effs) a ->
  Lang ctx (Logic ': Closure ': Quantifier ': Env ': effs) a
introduceEnv = \case
  Pure x -> pure x
  Op op k
    | OHere op' <- op -> Op (OHere op') k'
    | OThere (OHere op') <- op -> Op (OThere (OHere op')) k'
    | OThere (OThere (OHere op')) <- op -> Op (OThere (OThere (OHere op'))) k'
    | OThere (OThere (OThere op')) <- op -> Op (OThere (OThere (OThere (OThere op')))) k'
    where
      k' = introduceEnv . k
  Scoped scoped loom
    | SHere scoped' <- scoped -> Scoped (SHere scoped') loom'
    | SThere (SHere scoped') <- scoped -> Scoped (SThere (SHere scoped')) loom'
    | SThere (SThere (SHere scoped')) <- scoped -> Scoped (SThere (SThere (SHere scoped'))) loom'
    | SThere (SThere (SThere scoped')) <- scoped -> Scoped (SThere (SThere (SThere (SThere scoped')))) loom'
    where
      loom' = loom ~>~ hoist introduceEnv
