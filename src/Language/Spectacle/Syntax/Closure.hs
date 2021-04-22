{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RecordWildCards #-}

-- | Closures and variable relations.
--
-- @since 0.1.0.0
module Language.Spectacle.Syntax.Closure
  ( Closure (Closure),
    Effect (Close),
    (.=),
    runClosure,
  )
where

import Data.Coerce (coerce)
import Data.Void (absurd)

import Data.Functor.Loom (runLoom, weave)
import Data.Type.Rec (Name, Rec, setRec, type (#), type (.|))
import Language.Spectacle.Exception.RuntimeException (RuntimeException)
import Language.Spectacle.Lang
  ( Effect,
    Lang (Pure, Yield),
    Member,
    Members,
    Union (Op, Scoped),
    decomposeOp,
    decomposeS,
    scope,
  )
import Language.Spectacle.RTS.Registers
  ( Registers,
    Thunk (Evaluated, Thunk, Unchanged),
    emptyRegisters,
    getRegister,
    setRegister,
    setThunk,
  )
import Language.Spectacle.Syntax.Closure.Internal (Closure (Closure), Effect (Close))
import Language.Spectacle.Syntax.Error (Error, throwE)
import Language.Spectacle.Syntax.NonDet (NonDet, oneOf)
import Language.Spectacle.Syntax.Substitute
  ( Plain,
    Prime,
    RuntimeState (RuntimeState, callStack, plains, primes),
    runExpr,
  )

-- -------------------------------------------------------------------------------------------------

-- | Determines the value of a primed value in the next time frame. The following example allows for
-- the new value of "x" to be any 'Int' in the range @[1 .. 5]@.
--
-- >>> #x .= oneOf [1 :: Int .. 5 :: Int]
-- >>> prime #x
--
-- @since 0.1.0.0
(.=) ::
  (s # a .| ctx, Member Closure effs) =>
  Name s ->
  Lang ctx '[Prime, Plain, NonDet, Error RuntimeException] a ->
  Lang ctx effs ()
name .= expr = scope (Close name expr)

-- | Discharges a 'Closure' effect, returning a 'Rec' new values for each variable in @ctx@.
--
-- @since 0.1.0.0
runClosure ::
  Members '[NonDet, Error RuntimeException] effs =>
  Rec ctx ->
  Lang ctx (Closure ': effs) a ->
  Lang ctx effs (Rec ctx, a)
runClosure vars m = do
  rs <- registers . fst <$> makeThunks (emptyClosureState vars) m
  (st, x) <- evaluateThunks (ClosureState rs vars) m
  return (knownValues st, x)

-- | Evaluates all unevaluated closures in a specification.
--
-- @since 0.1.0.0
evaluateThunks ::
  Members '[NonDet, Error RuntimeException] effs =>
  ClosureState ctx ->
  Lang ctx (Closure ': effs) a ->
  Lang ctx effs (ClosureState ctx, a)
evaluateThunks cst = \case
  Pure x -> pure (cst, x)
  Yield (Op op) k -> case decomposeOp op of
    Left other -> Yield (Op other) (k' cst)
    Right bottom -> absurd (coerce bottom)
    where
      k' cst' = evaluateThunks cst' . k
  Yield (Scoped scoped loom) k -> case decomposeS scoped of
    Left other -> Yield (Scoped other loom') (uncurry k')
    Right (Close name _) -> case getRegister name (registers cst) of
      Thunk expr -> case runExpr (toRuntimeState cst) expr of
        Left exc -> throwE exc
        Right xs -> do
          (cst', x) <- runLoom loom' (pure ())
          (RuntimeState _ primes' _, x') <- oneOf xs
          k' (ClosureState (setRegister name x' primes') (setRec name x' (knownValues cst'))) x
      Evaluated x -> do
        (cst', x') <- runLoom loom' (pure ())
        k' (setKnown name x cst') x'
      Unchanged -> runLoom loom' (pure ()) >>= uncurry k'
    where
      k' cst' = evaluateThunks cst' . k

      loom' = weave (cst, ()) (uncurry evaluateThunks) loom

-- | Traverses 'Lang' collecting all closures as 'Thunks' which will subsequently be evaluated
-- by 'evaluateThunks'. An extra pass is needed to register all closures before substitution of
-- primed variables before substitution takes place.
--
-- @since 0.1.0.0
makeThunks ::
  Members (Error RuntimeException) effs =>
  ClosureState ctx ->
  Lang ctx (Closure ': effs) a ->
  Lang ctx effs (ClosureState ctx, a)
makeThunks cst = \case
  Pure x -> pure (cst, x)
  Yield (Op op) k -> case decomposeOp op of
    Left other -> Yield (Op other) (k' cst)
    Right bottom -> absurd (coerce bottom)
    where
      k' cst' = makeThunks cst' . k
  Yield (Scoped scoped loom) k -> case decomposeS scoped of
    Left other -> Yield (Scoped other loom') (uncurry k')
    Right (Close name expr) -> do
      (cst', x) <- runLoom loom' (pure ())
      k' (bindName name expr cst') x
    where
      k' cst' = makeThunks cst' . k

      loom' = weave (cst, ()) (uncurry makeThunks) loom

-- -------------------------------------------------------------------------------------------------

-- | Internal state carried by the handler for 'Closure'.
--
-- @since 0.1.0.0
data ClosureState ctx = ClosureState
  { registers :: Registers ctx
  , knownValues :: Rec ctx
  }

-- | Construct an empty 'ClosureState'.
--
-- @since 0.1.0.0
emptyClosureState :: Rec ctx -> ClosureState ctx
emptyClosureState r = ClosureState (emptyRegisters r) r

-- | Records the result of an evaluated thunk.
--
-- @since 0.1.0.0
setKnown :: s # a .| ctx => Name s -> a -> ClosureState ctx -> ClosureState ctx
setKnown name x ClosureState {..} =
  ClosureState
    { knownValues = setRec name x knownValues
    , ..
    }

-- | Injects a 'ClosuresState' into a 'RuntimeState' with an empty call stack.
--
-- @since 0.1.0.0
toRuntimeState :: ClosureState ctx -> RuntimeState ctx
toRuntimeState ClosureState {..} =
  RuntimeState
    { plains = knownValues
    , primes = registers
    , callStack = mempty
    }

-- | Binds a name to the RHS of a '(.=)'.
--
-- @since 0.1.0.0
bindName ::
  s # a .| ctx =>
  Name s ->
  Lang ctx '[Prime, Plain, NonDet, Error RuntimeException] a ->
  ClosureState ctx ->
  ClosureState ctx
bindName name expr ClosureState {..} =
  ClosureState
    { registers = setThunk name expr registers
    , ..
    }
