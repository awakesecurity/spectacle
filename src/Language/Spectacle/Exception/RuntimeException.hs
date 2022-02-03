{-# LANGUAGE DeriveAnyClass #-}

module Language.Spectacle.Exception.RuntimeException
  ( RuntimeException (VariableException, QuantifierException, UserException),
    VariableException (CyclicReference, Uninitialized),
    QuantifierException (ForallViolated, ExistsViolated),
  )
where

import Control.Exception (Exception)
import Type.Reflection (Typeable)

-- ---------------------------------------------------------------------------------------------------------------------

data RuntimeException where
  VariableException ::
    VariableException ->
    RuntimeException
  QuantifierException ::
    QuantifierException ->
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
  -- @since 1.0.0
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
  -- @since 1.0.0
  ExistsViolated ::
    QuantifierException
  deriving stock (Show, Typeable)
  deriving anyclass (Exception)
