-- | Closures and variable relations.
--
-- @since 0.1.0.0
module Language.Spectacle.Syntax.Closure
  ( Closure (Closure),
    Effect (Close),
    (.=),
    runActionClosure,
  )
where

import Data.Coerce (coerce)
import Data.Void (absurd)

import Data.Functor.Loom (hoist, runLoom, (~>~))
import Data.Type.Rec (Has, Name)
import qualified Data.Type.Rec as Rec
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
  ( RuntimeState (newValues),
    StateFun,
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

-- | The ('.=') operator relates the variable @s@ to the primed values it can access in the next temporal frame. For
-- example, a relation which increments a variable named "x" with any number 1 through 5 each frame of time would be
-- written as:
--
-- @
-- increment :: Action IncrementSpec Bool
-- increment = do
--   x <- plain #x -- retrieve the value of "x" from the previous frame
--   exists [1 .. 5] \n -> do
--     #x .= return (x + n) -- set the value of "x" in the next value to x + n for some number 1 <= n <= 5
--     return True -- relate the value to the next frame, @return (odd n)@ could be written to exclude even n.
-- @
--
-- @since 0.1.0.0
infix 4 .=

(.=) :: (Member Closure effs, Has s a ctx) => Name s -> StateFun ctx a -> Lang ctx effs ()
name .= expr = scope (Close name expr)
{-# INLINE (.=) #-}

-- | Discharges a 'Closure' effect, returning a 'Rec' new values for each variable in @ctx@.
--
-- @since 0.1.0.0
runActionClosure ::
  Members '[NonDet, Env, Error RuntimeException] effs =>
  Lang ctx (Closure ': effs) a ->
  Lang ctx effs a
runActionClosure m = evaluateThunks (makeThunks m)
{-# INLINE runActionClosure #-}

-- | Evaluates all unevaluated closures in a specification.
--
-- @since 0.1.0.0
evaluateThunks ::
  Members '[NonDet, Env, Error RuntimeException] effs =>
  Lang ctx (Closure ': effs) a ->
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
    Right (Close name _) ->
      gets (getRegister name . primes) >>= \case
        Thunk expr -> do
          x <- substitute name expr
          modify \rtst -> rtst {newValues = Rec.set name x (newValues rtst)}
          runLoom loom' (pure ())
        Evaluated x -> do
          modify \rtst -> rtst {newValues = Rec.set name x (newValues rtst)}
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
  Members '[Closure, Env, Error RuntimeException] effs =>
  Lang ctx effs a ->
  Lang ctx effs a
makeThunks = \case
  Pure x -> pure x
  Op op k -> case project op of
    Nothing -> Op op k'
    Just (Closure bottom :: Closure x) -> absurd bottom
    where
      k' = makeThunks . k
  Scoped scoped loom -> case projectS scoped of
    Nothing -> Scoped scoped loom'
    Just (Close name expr) -> do
      modify \rtst -> rtst {primes = setThunk name expr (primes rtst)}
      scope (Close name expr)
      runLoom loom' (pure ())
    where
      loom' = loom ~>~ hoist makeThunks
