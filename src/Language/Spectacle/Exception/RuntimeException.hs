{-# LANGUAGE DeriveAnyClass #-}

module Language.Spectacle.Exception.RuntimeException
  ( RuntimeException (VariableException, QuantifierException),
    VariableException (CyclicReference),
    QuantifierException (ForallViolated, ExistsViolated)
  )
where

import Control.Exception (Exception)
import Type.Reflection (Typeable)

-- -------------------------------------------------------------------------------------------------

data RuntimeException where
  VariableException :: VariableException -> RuntimeException
  QuantifierException :: QuantifierException -> RuntimeException
  deriving stock (Show, Typeable)
  deriving anyclass Exception

data VariableException where
  CyclicReference :: [String] -> VariableException
  deriving stock (Show, Typeable)
  deriving anyclass Exception

data QuantifierException where
  ForallViolated :: QuantifierException
  ExistsViolated :: QuantifierException
  deriving stock (Show, Typeable)
  deriving anyclass Exception
