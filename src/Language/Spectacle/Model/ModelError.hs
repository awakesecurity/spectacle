-- | Model checker errors.
--
-- @since 0.1.0.0
module Language.Spectacle.Model.ModelError
  ( -- * ModelError
    ModelError
      ( NoInitState,
        RuntimeError,
        FailAlways,
        FailEventually
      ),
  )
where

import Data.Kind

import Language.Spectacle.Exception.RuntimeException

-- ---------------------------------------------------------------------------------------------------------------------

data ModelError :: Type where
  NoInitState :: ModelError
  RuntimeError :: RuntimeException -> ModelError
  FailAlways :: String -> ModelError
  FailEventually :: String -> ModelError
  deriving (Show)
