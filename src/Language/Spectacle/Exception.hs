{-# LANGUAGE DeriveAnyClass #-}

module Language.Spectacle.Exception
  ( SpecException (RuntimeException, ModelCheckerException),
  )
where

import Control.Exception (Exception)
import Type.Reflection (Typeable)

import Language.Spectacle.Exception.ModelCheckerException ( ModelCheckerException)
import Language.Spectacle.Exception.RuntimeException ( RuntimeException)

-- ---------------------------------------------------------------------------------------------------------------------

data SpecException where
  RuntimeException :: RuntimeException -> SpecException
  ModelCheckerException :: ModelCheckerException -> SpecException
  deriving stock (Show, Typeable)
  deriving anyclass (Exception)
