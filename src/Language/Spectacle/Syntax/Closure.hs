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
import Language.Spectacle.Syntax.Closure.Internal (Closure (Closure), Effect (Close))
import Language.Spectacle.Syntax.Env (Env, gets, modify)
import Language.Spectacle.Syntax.Error (Error)
import Language.Spectacle.Syntax.NonDet (NonDet)
import Language.Spectacle.Syntax.Prime (RuntimeState (primes), substitute)

-- ---------------------------------------------------------------------------------------------------------------------

-- | Determines the value of a primed value in the next time frame. The following example allows for
-- the new value of "x" to be any 'Int' in the range @[1 .. 5]@.
--
-- >>> #x .= oneOf [1 :: Int .. 5 :: Int]
-- >>> prime #x
--
-- @since 0.1.0.0
(.=) :: (s # a .| ctx, Member Closure effs) => Name s -> RelationTerm ctx a -> Lang ctx effs ()
name .= expr = scope (Close name expr)
{-# INLINE (.=) #-}

-- | Discharges a 'Closure' effect, returning a 'Rec' new values for each variable in @ctx@.
--
-- @since 0.1.0.0
runClosure :: Members '[NonDet, Env, Error RuntimeException] effs => Lang ctx (Closure ': effs) a -> Lang ctx effs a
runClosure m = evaluateThunks (makeThunks m)
{-# INLINE runClosure #-}

-- | Evaluates all unevaluated closures in a specification.
--
-- @since 0.1.0.0
evaluateThunks :: Members '[NonDet, Env, Error RuntimeException] effs => Lang ctx (Closure ': effs) a -> Lang ctx effs a
evaluateThunks = \case
  Pure x -> pure x
  Op op k -> case decomposeOp op of
    Left other -> Op other k'
    Right bottom -> absurd (coerce bottom)
    where
      k' = evaluateThunks . k
  Scoped scoped loom -> case decomposeS scoped of
    Left other -> Scoped other loom'
    Right (Close name _) ->
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
makeThunks :: Members '[Closure, Env, Error RuntimeException] effs => Lang ctx effs a -> Lang ctx effs a
makeThunks = \case
  Pure x -> pure x
  Op op k -> case project op of
    Nothing -> Op op k'
    Just (Closure bottom) -> absurd bottom
    where
      k' = makeThunks . k
  Scoped scoped loom -> case projectS scoped of
    Nothing -> Scoped scoped loom'
    Just (Close name expr) -> do
      x <- runLoom loom' (pure ())
      modify \rtst -> rtst {primes = setThunk name expr (primes rtst)}
      scope (Close name expr)
      return x
    where
      loom' = loom ~>~ hoist makeThunks
