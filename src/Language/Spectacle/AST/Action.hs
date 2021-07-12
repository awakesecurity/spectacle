module Language.Spectacle.AST.Action
  ( -- * Temporal Actions
    type Action,
    type ActionSyntax,

    -- ** Interpreters
    runAction,
    rewriteLogic,
    applyComplement,
    introduceEnv,
  )
where

import Data.Function ((&))

import Data.Functor.Loom (hoist, runLoom, (~>~))
import Data.Type.Rec (Rec)
import Language.Spectacle.AST.Action.Internal (Action, ActionSyntax)
import Language.Spectacle.Exception.RuntimeException (RuntimeException)
import Language.Spectacle.Lang
  ( Lang (Op, Pure, Scoped),
    Member (projectS),
    Members,
    Op (OHere, OThere),
    Scoped (SHere, SThere),
    runLang,
  )
import Language.Spectacle.RTS.Registers (RuntimeState, emptyRuntimeState)
import Language.Spectacle.Syntax.Closure
  ( Closure,
    ClosureKind (ActionClosure),
    runActionClosure,
  )
import Language.Spectacle.Syntax.Env (Env, runEnv)
import Language.Spectacle.Syntax.Error (runError)
import Language.Spectacle.Syntax.Logic
  ( Effect (Complement, Conjunct, Disjunct),
    Logic,
    complement,
    conjunct,
    disjunct,
    runLogic,
  )
import Language.Spectacle.Syntax.NonDet (NonDet, runNonDetA)
import Language.Spectacle.Syntax.Plain (runPlain)
import Language.Spectacle.Syntax.Quantifier
  ( Effect (Exists, Forall),
    Quantifier,
    exists,
    forall,
    runQuantifier,
  )

-- ---------------------------------------------------------------------------------------------------------------------

-- | Completely evaluate a temporal action yielding either a 'RuntimeException' or a collection of new worlds accessible
-- by the action given.
--
-- @since 0.1.0.0
runAction :: Rec ctx -> Action ctx Bool -> Either RuntimeException [(RuntimeState ctx, Bool)]
runAction knowns nextState =
  nextState
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
{-# INLINE runAction #-}

-- | Traverses the effects in an action, rewriting all logical operators and quantifiers scoped within a negation.
--
-- @since 0.1.0.0
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
-- @since 0.1.0.0
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
-- @since 0.1.0.0
introduceEnv ::
  Lang ctx (Logic ': Closure 'ActionClosure ': Quantifier ': effs) a ->
  Lang ctx (Logic : Closure 'ActionClosure ': Quantifier ': Env ': effs) a
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
