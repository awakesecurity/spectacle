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
    substitute,
  )
where

import Data.Coerce (coerce)
import Data.Function ((&))
import Data.Void (absurd)

import Data.Functor.Loom (hoist, runLoom, (~>~))
import Data.Type.Rec (Name, Rec, getRec, type (#), type (.|))
import Language.Spectacle.Exception.RuntimeException
  ( RuntimeException (VariableException),
    VariableException (CyclicReference),
  )
import Language.Spectacle.Lang
  ( Effect,
    Lang (Op, Pure, Scoped),
    Member,
    Members,
    Op (OHere, OThere),
    Scoped (SHere, SThere),
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
    type RelationTermSyntax,
  )
import Language.Spectacle.Syntax.Env
  ( Env,
    get,
    gets,
    modify,
    put,
    runEnv,
  )
import Language.Spectacle.Syntax.Error (Error, runError, throwE)
import Language.Spectacle.Syntax.NonDet (NonDet, oneOf, runNonDetA)
import Language.Spectacle.Syntax.Plain (runPlain)
import Language.Spectacle.Syntax.Prime.Internal (Effect (PrimeVar), Prime (Prime))

-- -------------------------------------------------------------------------------------------------

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
  forall ctx effs a.
  Members '[Env, Error RuntimeException, NonDet] effs =>
  Lang ctx (Prime ': effs) a ->
  Lang ctx effs a
runPrime = \case
  Pure x -> pure x
  Op op k -> case decomposeOp op of
    Left other -> Op other (runPrime . k)
    Right bottom -> absurd (coerce bottom)
  Scoped scoped loom -> do
    case decomposeS scoped of
      Left other -> Scoped other loomPrime
      Right (PrimeVar name) -> do
        rst <- get
        case getRegister name (primes rst) of
          Thunk expr ->
            if show name `elem` callStack rst
              then throwE (VariableException (CyclicReference (callStack rst)))
              else do
                x <- substitute name expr
                modify \rst' ->
                  rst' {primes = setRegister name x (primes rst')}
                runLoom loomPrime (pure x)
          Evaluated x -> runLoom loomPrime (pure x)
          Unchanged -> do
            rst' <- gets (getRec name . plains)
            runLoom loomPrime (pure rst')
    where
      loomPrime = loom ~>~ hoist runPrime

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
    Left other -> Scoped other loomSubstPrime
    Right (PrimeVar name) -> do
      let x = getRec name vars
      runLoom loomSubstPrime (pure x)
    where
      loomSubstPrime = loom ~>~ hoist (substPrime vars)

-- | Evaluates the thunk for a primed variable.
--
-- @since 0.1.0.0
substitute ::
  (Members '[Env, NonDet, Error RuntimeException] effs, s # a .| ctx) =>
  Name s ->
  Lang ctx RelationTermSyntax a ->
  Lang ctx effs a
substitute name expr = do
  rst <- get
  let result =
        expr
          & introduceState
          & runPrime
          & runEnv rst
          & runPlain (plains rst)
          & runNonDetA @[]
          & runError
          & runLang
  case result of
    Left e -> throwE e
    Right results -> do
      (rst', x) <- oneOf results
      put (rst' {primes = setRegister name x (primes rst')})
      pure x
  where
    introduceState ::
      Lang ctx (Prime : effs) a ->
      Lang ctx (Prime ': Env ': effs) a
    introduceState = \case
      Pure x -> pure x
      Op op k
        | OHere op' <- op -> Op (OHere op') k'
        | OThere op' <- op -> Op (OThere (OThere op')) k'
        where
          k' = introduceState . k
      Scoped scoped loom
        | SHere scoped' <- scoped -> Scoped (SHere scoped') loom'
        | SThere scoped' <- scoped -> Scoped (SThere (SThere scoped')) loom'
        where
          loom' = loom ~>~ hoist introduceState
