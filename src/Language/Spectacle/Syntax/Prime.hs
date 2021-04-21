-- | Prime or time variable usage and substitution.
--
-- @since 0.1.0.0
module Language.Spectacle.Syntax.Prime
  ( Prime (Prime),
    Effect (PrimeVar),
    prime,
    runPrime,
    RuntimeState (RuntimeState, plains, primes, callStack),
    runExpr,
  )
where

import Data.Coerce (coerce)
import Data.Function ((&))
import Data.Void (absurd)

import Data.Functor.Loom (runLoom, weave)
import Data.Type.Rec (Name, getRec, type (#), type (.|))
import Language.Spectacle.Exception.RuntimeException
  ( RuntimeException (VariableException),
    VariableException (CyclicReference),
  )
import Language.Spectacle.Lang
  ( Effect,
    Lang (Pure, Yield),
    Member,
    Members,
    Union (Op, Scoped),
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
  Members '[Error RuntimeException, NonDet] effs =>
  RuntimeState ctx ->
  Lang ctx (Prime ': effs) a ->
  Lang ctx effs (RuntimeState ctx, a)
runPrime rst = \case
  Pure x -> pure (rst, x)
  Yield (Op op) k -> case decomposeOp op of
    Left other -> Yield (Op other) (runPrime rst . k)
    Right bottom -> absurd (coerce bottom)
  Yield (Scoped scoped loom) k -> case decomposeS scoped of
    Left other -> Yield (Scoped other loom') (uncurry k')
    Right (PrimeVar name) -> case getRegister name (primes rst) of
      Thunk expr ->
        if show name `elem` callStack rst
          then throwE (VariableException (CyclicReference (callStack rst)))
          else do
            (rst', x) <- substitute rst name expr
            runLoom loom' (pure x) >>= (k' rst' . snd)
      Evaluated x -> runLoom loom' (pure x) >>= uncurry k'
      Unchanged -> do
        let x = getRec name (plains rst)
        runLoom loom' (pure x) >>= uncurry k'
    where
      k' rst' = runPrime rst' . k

      loom' = weave (rst, ()) (uncurry runPrime) loom

-- | Evaluates the thunk for a primed variable.
--
-- @since 0.1.0.0
substitute ::
  (Members '[NonDet, Error RuntimeException] effs, s # a .| ctx) =>
  RuntimeState ctx ->
  Name s ->
  Lang ctx '[Prime, Plain, NonDet, Error RuntimeException] a ->
  Lang ctx effs (RuntimeState ctx, a)
substitute rst name expr =
  case runExpr (rst {callStack = show name : callStack rst}) expr of
    Left e -> throwE e
    Right results -> do
      (rst', x) <- oneOf results
      return (rst {primes = setRegister name x (primes rst')}, x)

-- | Run an expression on the right-hand side of a primed variable relation.
--
-- @since 0.1.0.0
runExpr ::
  RuntimeState ctx ->
  Lang ctx '[Prime, Plain, NonDet, Error RuntimeException] x ->
  Either RuntimeException [(RuntimeState ctx, x)]
runExpr rst rvalue =
  rvalue
    & runPrime rst
    & runPlain (plains rst)
    & runNonDetA @[]
    & runError
    & runLang
