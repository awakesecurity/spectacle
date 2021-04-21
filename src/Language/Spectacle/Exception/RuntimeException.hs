{-# LANGUAGE DeriveAnyClass #-}

module Language.Spectacle.Exception.RuntimeException
  ( RuntimeException (VariableException),
    VariableException (CyclicReference),
  )
where

import Control.Exception (Exception)
import Type.Reflection (Typeable)

-- -------------------------------------------------------------------------------------------------

data RuntimeException where
  VariableException :: VariableException -> RuntimeException
  deriving stock (Show, Typeable)
  deriving anyclass Exception

data VariableException where
  CyclicReference :: [String] -> VariableException
  deriving stock (Show, Typeable)
  deriving anyclass Exception
