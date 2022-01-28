{-# LANGUAGE GeneralizedNewtypeDeriving #-}

-- |
--
-- @since 0.1.0.0
module Language.Spectacle.Model.Monad
  ( -- * Model Monad
    ModelIO,

    -- * Model Transformer
    ModelM (ModelM),
    unModelM,
    runModelM,

    -- * Re-exports
    module Language.Spectacle.Model.ModelEnv,
    module Language.Spectacle.Model.ModelError,
    module Language.Spectacle.Model.ModelState,
  )
where

import Control.Monad.Except (ExceptT (ExceptT), MonadError, runExceptT)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Reader (MonadReader, ReaderT (ReaderT), runReaderT)
import Control.Monad.Ref (RefM, runRefM)
import Control.Monad.State (MonadState)
import Data.Function ((&))

import Language.Spectacle.Model.ModelEnv
import Language.Spectacle.Model.ModelError
import Language.Spectacle.Model.ModelState

-- ---------------------------------------------------------------------------------------------------------------------

type ModelIO ctx = ModelM ctx IO

newtype ModelM ctx m a = ModelM
  {unModelM :: ExceptT ModelError (ReaderT ModelEnv (RefM (ModelState ctx) m)) a}
  deriving (Functor, Applicative, Monad)

runModelM :: MonadIO m => ModelM ctx m a -> ModelEnv -> m (ModelState ctx, Either ModelError a)
runModelM model env =
  unModelM model
    & runExceptT
    & flip runReaderT env
    & flip runRefM mempty

-- | @since 0.1.0.0
deriving instance MonadIO m => MonadState (ModelState ctx) (ModelM ctx m)

-- | @since 0.1.0.0
deriving instance Monad m => MonadReader ModelEnv (ModelM ctx m)

-- | @since 0.1.0.0
deriving instance Monad m => MonadError ModelError (ModelM ctx m)
