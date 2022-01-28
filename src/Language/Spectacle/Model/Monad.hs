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
  ( ModelEnv,
    actionInfo,
    fairnessOf,
    modalityOf,
    newModelEnv,
    strongFairActions,
    unfairActions,
    weakFairActions,
  )
import Language.Spectacle.Model.ModelError (ModelError)
import Language.Spectacle.Model.ModelState
  ( ModelState,
    enabledActionsAt,
    indexNode,
    member,
    queuedActionsAt,
  )

-- ---------------------------------------------------------------------------------------------------------------------

type ModelIO ctx = ModelM ctx IO

newtype ModelM ctx m a = ModelM
  {unModelM :: ExceptT ModelError (ReaderT ModelEnv (RefM (ModelState ctx) m)) a}
  deriving (Functor, Applicative, Monad)

runModelM ::
  MonadIO m =>
  ModelM ctx m a ->
  ModelEnv ->
  ModelState ctx ->
  m (ModelState ctx, Either ModelError a)
runModelM model env st =
  unModelM model
    & runExceptT
    & flip runReaderT env
    & flip runRefM st

-- | @since 0.1.0.0
deriving instance MonadIO m => MonadState (ModelState ctx) (ModelM ctx m)

-- | @since 0.1.0.0
deriving instance Monad m => MonadReader ModelEnv (ModelM ctx m)

-- | @since 0.1.0.0
deriving instance Monad m => MonadError ModelError (ModelM ctx m)
