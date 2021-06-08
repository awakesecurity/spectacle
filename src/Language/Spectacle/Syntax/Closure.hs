-- | Closures and variable relations.
--
-- @since 0.1.0.0
module Language.Spectacle.Syntax.Closure
  ( ClosureKind (ActionClosure, InitialClosure),
    Closure (Closure),
    Effect (CloseAction, CloseInitial),
    define,
    (.=),
    runActionClosure,
  )
where

import Data.Coerce (coerce)
import Data.Void (absurd)

import Data.Functor.Loom (hoist, runLoom, (~>~))
import Data.Type.Rec (Name, setRec, type (#), type (.|))
import Language.Spectacle.Exception.RuntimeException (RuntimeException)
import Language.Spectacle.Lang
  ( Effect,
    Lang (Op, Pure, Scoped),
    Member (project, projectS),
    Members,
    decomposeOp,
    decomposeS,
    scope,
  )
import Language.Spectacle.RTS.Registers
  ( RelationTerm,
    RuntimeState (newValues),
    Thunk (Evaluated, Thunk, Unchanged),
    getRegister,
    setThunk,
  )
import Language.Spectacle.Syntax.Closure.Internal
  ( Closure (Closure),
    ClosureKind (ActionClosure, InitialClosure),
    Effect (CloseAction, CloseInitial),
  )
import Language.Spectacle.Syntax.Env (Env, gets, modify)
import Language.Spectacle.Syntax.Error (Error)
import Language.Spectacle.Syntax.NonDet (NonDet)
import Language.Spectacle.Syntax.Prime (RuntimeState (primes), substitute)

-- ---------------------------------------------------------------------------------------------------------------------

define ::
  (s # a .| ctx, Member (Closure 'InitialClosure) effs') =>
  Name s ->
  Lang ctx '[NonDet] a ->
  Lang ctx effs' ()
define name expr = scope (CloseInitial name expr)
{-# INLINE define #-}

(.=) ::
  (s # a .| ctx, Member (Closure 'ActionClosure) effs') =>
  Name s ->
  RelationTerm ctx a ->
  Lang ctx effs' ()
name .= expr = scope (CloseAction name expr)
{-# INLINE (.=) #-}

-- | Discharges a 'Closure' effect, returning a 'Rec' new values for each variable in @ctx@.
--
-- @since 0.1.0.0
runActionClosure ::
  Members '[NonDet, Env, Error RuntimeException] effs =>
  Lang ctx (Closure 'ActionClosure ': effs) a ->
  Lang ctx effs a
runActionClosure m = evaluateThunks (makeThunks m)
{-# INLINE runActionClosure #-}

-- | Evaluates all unevaluated closures in a specification.
--
-- @since 0.1.0.0
evaluateThunks ::
  Members '[NonDet, Env, Error RuntimeException] effs =>
  Lang ctx (Closure 'ActionClosure ': effs) a ->
  Lang ctx effs a
evaluateThunks = \case
  Pure x -> pure x
  Op op k -> case decomposeOp op of
    Left other -> Op other k'
    Right bottom -> absurd (coerce bottom)
    where
      k' = evaluateThunks . k
  Scoped scoped loom -> case decomposeS scoped of
    Left other -> Scoped other loom'
    Right (CloseAction name _) ->
      gets (getRegister name . primes) >>= \case
        Thunk expr -> do
          x <- substitute name expr
          modify \rtst -> rtst {newValues = setRec name x (newValues rtst)}
          runLoom loom' (pure ())
        Evaluated x -> do
          modify \rtst -> rtst {newValues = setRec name x (newValues rtst)}
          runLoom loom' (pure ())
        Unchanged -> runLoom loom' (pure ())
    where
      loom' = loom ~>~ hoist evaluateThunks

-- | Traverses 'Lang' collecting all closures as 'Thunks' which will subsequently be evaluated
-- by 'evaluateThunks'. An extra pass is needed to register all closures before substitution of
-- primed variables takes place.
--
-- @since 0.1.0.0
makeThunks ::
  Members '[Closure 'ActionClosure, Env, Error RuntimeException] effs =>
  Lang ctx effs a ->
  Lang ctx effs a
makeThunks = \case
  Pure x -> pure x
  Op op k -> case project op of
    Nothing -> Op op k'
    Just (Closure bottom :: Closure 'ActionClosure x) -> absurd bottom
    where
      k' = makeThunks . k
  Scoped scoped loom -> case projectS scoped of
    Nothing -> Scoped scoped loom'
    Just (CloseAction name expr) -> do
      x <- runLoom loom' (pure ())
      modify \rtst -> rtst {primes = setThunk name expr (primes rtst)}
      scope (CloseAction name expr)
      return x
    where
      loom' = loom ~>~ hoist makeThunks
