{-# LANGUAGE TupleSections #-}

module Language.Spectacle.AST.Initial
  ( -- * Initial State Syntax
    type Initial,
    type InitialSyntax,

    -- ** Interpreters
    runInitial,
    runInitialClosure,
  )
where

import Data.Coerce (coerce)
import Data.Function ((&))
import Data.Void (absurd)

import Data.Functor.Loom (runLoom, weave, (~>~))
import Data.Type.Rec
  ( Rec,
    RecT (RConT, RNil),
    ReflectRow,
    pattern RCon,
  )
import Language.Spectacle.AST.Initial.Internal
  ( Initial,
    InitialSyntax,
    InitialValues (InitialValues),
    emptyInitialValues,
    initializeValue,
  )
import Language.Spectacle.Exception.RuntimeException
  ( RuntimeException (VariableException),
    VariableException (Uninitialized),
  )
import Language.Spectacle.Lang
  ( Lang (Op, Pure, Scoped),
    Member,
    Members,
    decomposeOp,
    decomposeS,
    runLang,
  )
import Language.Spectacle.Syntax.Closure
  ( Closure (Closure),
    ClosureKind (InitialClosure),
    Effect (CloseInitial),
  )
import Language.Spectacle.Syntax.Error (Error, runError, throwE)
import Language.Spectacle.Syntax.NonDet (NonDet, foldMapA, runNonDetA)

-- ---------------------------------------------------------------------------------------------------------------------

-- | Runs an 'Initial' expression type, producing the initial values we can check a model with.
--
-- @since 0.1.0.0
runInitial :: ReflectRow ctx => Initial ctx a -> Either RuntimeException [Rec ctx]
runInitial initialAction =
  initialAction
    & runInitialClosure emptyInitialValues
    & (>>= \(rs, _) -> unpackInitials rs)
    & runNonDetA
    & runError
    & runLang
  where
    unpackInitials ::
      Members '[Error RuntimeException, NonDet] effs =>
      InitialValues ctx' ->
      Lang ctx effs (Rec ctx')
    unpackInitials (InitialValues RNil) = return RNil
    unpackInitials (InitialValues (RConT name value r)) = case value of
      Nothing -> throwE (VariableException (Uninitialized (show name)))
      Just x -> do
        r' <- unpackInitials (InitialValues r)
        return (RCon name x r')
{-# INLINE runInitial #-}

-- | Specialized interpreter which discharges a 'Closure' effect in an 'Initial' action.
--
-- @since 0.1.0.0
runInitialClosure ::
  Member NonDet effs =>
  InitialValues ctx ->
  Lang ctx (Closure 'InitialClosure ': effs) a ->
  Lang ctx effs (InitialValues ctx, a)
runInitialClosure initials = \case
  Pure x -> pure (initials, x)
  Op op k -> case decomposeOp op of
    Left other -> Op other (runInitialClosure initials . k)
    Right bottom -> absurd (coerce bottom)
  Scoped scoped loom -> case decomposeS scoped of
    Left other -> Scoped other (loom' initials)
    Right (CloseInitial name expr) ->
      let recs =
            expr
              & runNonDetA @[]
              & runLang
       in flip foldMapA recs \x ->
            runLoom (loom' (initializeValue name x initials)) (pure ())
    where
      loom' initials' = loom ~>~ weave (initials', ()) (uncurry runInitialClosure)
{-# INLINE runInitialClosure #-}
