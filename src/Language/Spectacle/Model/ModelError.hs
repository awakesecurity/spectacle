-- | Model checker errors.
--
-- @since 0.1.0.0
module Language.Spectacle.Model.ModelError
  ( -- * ModelError
    ModelError
      ( NoInitState,
        RuntimeError,
        FailAlways,
        FailEventually,
        FailInfinitely,
        FailStays
      ),
  )
where

import Data.Kind (Type)

import Language.Spectacle.Exception.RuntimeException (RuntimeException)

-- ---------------------------------------------------------------------------------------------------------------------

data ModelError :: Type where
  NoInitState :: ModelError
  RuntimeError :: RuntimeException -> ModelError
  FailAlways :: String -> ModelError
  FailEventually :: String -> ModelError
  FailInfinitely :: String -> ModelError
  FailStays :: String -> ModelError
  deriving (Show)
