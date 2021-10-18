{-# LANGUAGE GeneralizedNewtypeDeriving #-}

-- |
--
-- @since 0.1.0.0
module Language.Spectacle.Model.Internal
  ( -- * Model Monad Transformer
    ModelT (ModelT, runModelT),
  )
where

import Control.Applicative (Alternative)
import Control.Monad.Except (ExceptT, MonadError)
import Control.Monad.State.Strict (MonadState, StateT)
import Control.Monad.Trans.Class (MonadTrans, lift)
import Data.Kind (Type)

import Control.Monad.Levels (LevelsT (LevelsT))
import Data.Context (Context)
import Language.Spectacle.Checker.MCError (MCError)
import Language.Spectacle.Model.MCState (MCState)

-- ---------------------------------------------------------------------------------------------------------------------

newtype ModelT :: Context -> (Type -> Type) -> Type -> Type where
  ModelT ::
    { runModelT ::
        LevelsT (StateT MCState (ExceptT [MCError ctxt] m)) a
    } ->
    ModelT ctxt m a
  deriving (Functor, Applicative, Monad, Alternative)
  deriving (MonadState MCState)

-- | @since 0.1.0.0
deriving instance
  Monad m => MonadError [MCError ctxt] (ModelT ctxt m)

-- | @since 0.1.0.0
instance MonadTrans (ModelT ctxt) where
  lift = ModelT . lift . lift . lift
  {-# INLINE lift #-}
