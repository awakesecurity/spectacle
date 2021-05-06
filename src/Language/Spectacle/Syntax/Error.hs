-- | The 'Error' effect for throwing exceptions which can be caught.
--
-- @since 0.1.0.0
module Language.Spectacle.Syntax.Error
  ( Error (ThrowE),
    Effect (CatchE),
    throwE,
    catchE,
    runError,
  )
where

import Data.Functor.Loom (hoist, (~>~), runLoom)
import Language.Spectacle.Lang
  ( Effect,
    Lang (Pure, Op, Scoped),
    Member,
    decomposeOp,
    decomposeS,
    scope,
    send,
  )
import Language.Spectacle.Syntax.Error.Internal (Effect (CatchE), Error (ThrowE))

-- -------------------------------------------------------------------------------------------------

-- | Throw an error of type @e@, escaping the current continuation up to the nearest enclosing
-- 'catchE'.
--
-- @since 0.1.0.0
throwE :: Member (Error e) effs => e -> Lang ctx effs a
throwE e = send (ThrowE e)

-- | Catch an error of type @e@ continuting from the provided function if an error was thrown.
--
-- @since 0.1.0.0
catchE :: Member (Error e) effs => Lang ctx effs a -> (e -> Lang ctx effs a) -> Lang ctx effs a
catchE m f = scope (CatchE m f)

-- | Discharge an 'Error' effect into either an error or the result of a successful computation.
--
-- @since 0.1.0.0
runError :: Lang ctx (Error e ': effs) a -> Lang ctx effs (Either e a)
runError = \case
  Pure x -> pure (Right x)
  Op op k -> case decomposeOp op of
    Left other -> Op other (runError . k)
    Right (ThrowE exc) -> pure (Left exc)
  Scoped scoped loom -> case decomposeS scoped of
    Left other -> Scoped other loom'
    Right (CatchE m catch) -> do
      x <- runLoom loom' m
      either (runLoom loom' . catch) (pure . pure) x
    where
      loom' = loom ~>~ hoist runError
