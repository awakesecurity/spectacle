{-# LANGUAGE DeriveAnyClass #-}

module Language.Spectacle.Exception.RuntimeException
  ( RuntimeException (VariableException, QuantifierException, SyntaxException, UserException),
    VariableException (CyclicReference, Uninitialized),
    QuantifierException (ForallViolated, ExistsViolated),
    SyntaxException (LevelMismatch),
  )
where

import Control.Exception (Exception)
import Type.Reflection (Typeable)

import Language.Spectacle.Syntax.Modal.Level (ExprLevel)

-- ---------------------------------------------------------------------------------------------------------------------

data RuntimeException where
  VariableException ::
    VariableException ->
    RuntimeException
  QuantifierException ::
    QuantifierException ->
    RuntimeException
  SyntaxException ::
    SyntaxException ->
    RuntimeException
  UserException ::
    String ->
    RuntimeException
  deriving stock (Show, Typeable)
  deriving anyclass (Exception)

data VariableException where
  CyclicReference :: [String] -> VariableException
  Uninitialized :: String -> VariableException
  deriving stock (Show, Typeable)
  deriving anyclass (Exception)

data QuantifierException where
  -- | 'ForallViolated' is thrown when a universally quantified set has one or more values that do not satisfy the
  -- predicate given to 'forall'.
  --
  -- @
  -- myAct :: Action '[] Bool
  -- myAct = forall [2] odd
  -- @
  --
  -- would throw 'ForallViolated' since 2 is not 'odd'.
  --
  -- @since 0.1.0.0
  ForallViolated ::
    QuantifierException
  -- | 'ExistsViolated' is thrown when an existentially quantified set has no values that satisfy its predicate,
  -- e.g. the expression
  --
  -- @
  -- myAct :: Action '[] Bool
  -- myAct = exists [2, 4] odd
  -- @
  --
  -- would throw 'ExistsViolated' since there is no number @n in {2, 4}@ such that @'odd' n ≡ 'True'@
  --
  -- @since 0.1.0.0
  ExistsViolated ::
    QuantifierException
  deriving stock (Show, Typeable)
  deriving anyclass (Exception)

data SyntaxException where
  LevelMismatch :: ExprLevel -> ExprLevel -> SyntaxException
  deriving stock (Show, Typeable)
  deriving anyclass (Exception)
