-- | Prime (or time) variable usage and substitution.
--
-- @since 0.1.0.0
module Language.Spectacle.Syntax.Prime
  ( -- * Labels
    Prime (Prime),
    Effect (PrimeVar),

    -- * Syntax
    prime,

    -- * Interpreters
    runPrime,
    substPrime,
    RuntimeState (RuntimeState, plains, primes, callStack),
    type RelationTerm,
    type RelationTermSyntax,
    runRelationExpr,
  )
where

import Data.Coerce (coerce)
import Data.Function ((&))
import Data.Kind (Type)
import Data.Void (absurd)
import GHC.TypeLits (Symbol)

import Data.Functor.Loom (hoist, runLoom, weave, (~>~))
import Data.Type.Rec (Ascribe, Name, Rec, getRec, type (#), type (.|))
import Language.Spectacle.Exception.RuntimeException
  ( RuntimeException (VariableException),
    VariableException (CyclicReference),
  )
import Language.Spectacle.Lang
  ( Effect,
    EffectK,
    Lang (Op, Pure, Scoped),
    Member,
    Members,
    decomposeOp,
    decomposeS,
    runLang,
    scope,
  )
import Language.Spectacle.RTS.Registers
  ( RuntimeState (RuntimeState, callStack, plains, primes),
    Thunk (Evaluated, Thunk, Unchanged),
    getRegister,
    setRegister,
  )
import Language.Spectacle.Syntax.Error (Error, runError, throwE)
import Language.Spectacle.Syntax.NonDet (NonDet, oneOf, runNonDetA)
import Language.Spectacle.Syntax.Plain (Plain, runPlain)
import Language.Spectacle.Syntax.Prime.Internal
  ( Effect (PrimeVar),
    Prime (Prime),
  )

-- -------------------------------------------------------------------------------------------------

type RelationTerm :: [Ascribe Symbol Type] -> Type -> Type
type RelationTerm ctx a = Lang ctx RelationTermSyntax a

type RelationTermSyntax :: [EffectK]
type RelationTermSyntax = '[Prime, Plain, NonDet, Error RuntimeException]

-- | 'prime' for a variable named @s@ is the value of @s@ in the next time frame.
--
-- @since 0.1.0.0
prime :: (s # a .| ctx, Member Prime effs) => Name s -> Lang ctx effs a
prime name = scope (PrimeVar name)
{-# INLINE prime #-}

-- | Discharges a 'Prime' effect. This interpreter carries out the substitution of primed variables
-- using a call-by-need evaluation strategy.
--
-- @since 0.1.0.0
runPrime ::
  Members '[Error RuntimeException, NonDet] effs =>
  RuntimeState RelationTermSyntax ctx ->
  Lang ctx (Prime ': effs) a ->
  Lang ctx effs (RuntimeState RelationTermSyntax ctx, a)
runPrime rst = \case
  Pure x -> pure (rst, x)
  Op op k -> case decomposeOp op of
    Left other -> Op other (runPrime rst . k)
    Right bottom -> absurd (coerce bottom)
  Scoped scoped loom -> case decomposeS scoped of
    Left other -> Scoped other (loom' rst)
    Right (PrimeVar name) -> case getRegister name (primes rst) of
      Thunk expr ->
        if show name `elem` callStack rst
          then throwE (VariableException (CyclicReference (callStack rst)))
          else do
            (rst', x) <- substitute rst name expr
            runLoom (loom' rst') (pure x)
      Evaluated x -> runLoom (loom' rst) (pure x)
      Unchanged -> do
        let x = getRec name (plains rst)
        runLoom (loom' rst) (pure x)
    where
      loom' rst' = loom ~>~ weave (rst', ()) (uncurry runPrime)

-- | Alternative interpreter for 'Prime' that substitutes with a 'Rec' rather than a record of
-- thunks.
--
-- @since 0.1.0.0
substPrime :: Rec ctx -> Lang ctx (Prime ': effs) a -> Lang ctx effs a
substPrime vars = \case
  Pure x -> pure x
  Op op k -> case decomposeOp op of
    Left other -> Op other (substPrime vars . k)
    Right bottom -> absurd (coerce bottom)
  Scoped scoped loom -> case decomposeS scoped of
    Left other -> Scoped other loom'
    Right (PrimeVar name) -> do
      let x = getRec name vars
      runLoom loom' (pure x)
    where
      loom' = loom ~>~ hoist (substPrime vars)

-- | Evaluates the thunk for a primed variable.
--
-- @since 0.1.0.0
substitute ::
  (Members '[NonDet, Error RuntimeException] effs, s # a .| ctx) =>
  RuntimeState RelationTermSyntax ctx ->
  Name s ->
  Lang ctx RelationTermSyntax a ->
  Lang ctx effs (RuntimeState RelationTermSyntax ctx, a)
substitute rst name expr =
  case runRelationExpr (rst {callStack = show name : callStack rst}) expr of
    Left e -> throwE e
    Right results -> do
      (rst', x) <- oneOf results
      return (rst {primes = setRegister name x (primes rst')}, x)

-- | Run an expression on the right-hand side of a primed variable relation.
--
-- @since 0.1.0.0
runRelationExpr ::
  RuntimeState RelationTermSyntax ctx ->
  RelationTerm ctx a ->
  Either RuntimeException [(RuntimeState RelationTermSyntax ctx, a)]
runRelationExpr rst rvalue =
  rvalue
    & runPrime rst
    & runPlain (plains rst)
    & runNonDetA @[]
    & runError
    & runLang
