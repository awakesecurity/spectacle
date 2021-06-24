{-# LANGUAGE DeriveAnyClass #-}

module Language.Spectacle.Exception.RuntimeException
  ( RuntimeException (VariableException, QuantifierException, SyntaxException, UserException),
    VariableException (CyclicReference, Uninitialized),
    QuantifierException (ForallViolated, ExistsViolated),
    SyntaxException (LevelMismatch, ComplementInL3),
  )
where

import Control.Exception (Exception)
import Type.Reflection (Typeable)

import Language.Spectacle.Syntax.Modal.Level ( ExprLevel )

-- ---------------------------------------------------------------------------------------------------------------------

data RuntimeException where
  VariableException :: VariableException -> RuntimeException
  QuantifierException :: QuantifierException -> RuntimeException
  SyntaxException :: SyntaxException -> RuntimeException
  UserException :: String -> RuntimeException
  deriving stock (Show, Typeable)
  deriving anyclass (Exception)

data VariableException where
  CyclicReference :: [String] -> VariableException
  Uninitialized :: String -> VariableException
  deriving stock (Show, Typeable)
  deriving anyclass (Exception)

data QuantifierException where
  ForallViolated :: QuantifierException
  ExistsViolated :: QuantifierException
  deriving stock (Show, Typeable)
  deriving anyclass (Exception)

data SyntaxException where
  LevelMismatch :: ExprLevel -> ExprLevel -> SyntaxException
  ComplementInL3 :: SyntaxException
  deriving stock (Show, Typeable)
  deriving anyclass (Exception)
