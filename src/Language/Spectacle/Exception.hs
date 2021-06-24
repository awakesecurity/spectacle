{-# LANGUAGE DeriveAnyClass #-}

module Language.Spectacle.Exception
  ( SpecException (RuntimeException, ModelCheckerException),
  )
where

import Control.Exception (Exception)
import Type.Reflection (Typeable)

import Data.Type.Rec (Rec)
import Language.Spectacle.Exception.ModelCheckerException (ModelCheckerException)
import Language.Spectacle.Exception.RuntimeException (RuntimeException)
import Language.Spectacle.Spec.Behavior (Behavior)

-- ---------------------------------------------------------------------------------------------------------------------

data SpecException where
  RuntimeException :: Show (Rec ctx) => Behavior ctx -> RuntimeException -> SpecException
  ModelCheckerException :: Show (Rec ctx) => Behavior ctx -> ModelCheckerException -> SpecException
  deriving stock (Typeable)
  deriving anyclass (Exception)

instance Show SpecException where
  show = \case
    RuntimeException _ exc -> show exc
    ModelCheckerException _ exc -> show exc
