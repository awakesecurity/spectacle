{-# LANGUAGE GeneralizedNewtypeDeriving #-}

-- |
--
-- @since 0.1.0.0
module Language.Spectacle.Model.Internal
  ( -- * Search Monad Transformer
    SearchT (SearchT, runSearchT),
    wrapSearchT,
    actionInfoView,
    isMarkedView,

    -- * Model Monad Transformer
    ModelT (ModelT),
    runModelT,

    -- * Base Monad
    BaseT (BaseT, runBaseT),

    -- * Model States
    MCState (MCState),

    -- * Model Environments
    MCEnv (MCEnv),
    _propInfo,

    -- ** Lenses
    mcStateCoverage,
    mcEnvPropInfo,  )
where

import Control.Applicative (Alternative)
import Control.Monad.Except (ExceptT, MonadError, throwError)
import Control.Monad.Logic
import Control.Monad.Reader (MonadReader, ReaderT)
import Control.Monad.State.Strict (MonadState, StateT)
import Control.Monad.Zip
import Data.Kind (Type)
import qualified Data.Map.Strict as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Lens.Micro.Mtl (use, view)

import Control.Monad.Levels (LevelsT (LevelsT), liftLevelsT, wrapLevelsT)
import Data.Context (Context)
import Data.World
import Language.Spectacle.Checker.MCError (MCError (MCInternalPropInfoError))
import Language.Spectacle.Model.MCEnv (MCEnv (MCEnv, _propInfo), mcEnvPropInfo)
import Language.Spectacle.Model.MCState (MCState (MCState), mcStateCoverage, mcStateIsMarked)
import Language.Spectacle.Specification (PropInfo)

-- ---------------------------------------------------------------------------------------------------------------------

newtype SearchT :: Context -> (Type -> Type) -> Type -> Type where
  SearchT :: {runSearchT :: LevelsT (BaseT ctxt m) a} -> SearchT ctxt m a
  deriving (Functor, Applicative, Monad, MonadZip)
  deriving (MonadReader MCEnv, MonadState MCState, MonadIO)

-- | @since 0.1.0.0
deriving instance
  Monad m => MonadError [MCError ctxt] (SearchT ctxt m)

-- | @since 0.1.0.0
deriving via
  LevelsT (BaseT ctxt m)
  instance
    Monad m => Alternative (SearchT ctxt m)

-- | @since 0.1.0.0
instance MonadTrans (SearchT ctxt) where
  lift = SearchT . lift . lift
  {-# INLINE lift #-}

wrapSearchT :: Monad m => BaseT ctxt m (SearchT ctxt m a) -> SearchT ctxt m a
wrapSearchT m = SearchT (liftLevelsT (fmap runSearchT m))

isMarkedView :: MonadState MCState m => World ctxt -> m Bool
isMarkedView = use . mcStateIsMarked
{-# INLINE isMarkedView #-}

actionInfoView :: (MonadError [MCError ctxt] m, MonadReader MCEnv m) => String -> m PropInfo
actionInfoView actIdent = do
  propInfo <- Map.lookup actIdent <$> view mcEnvPropInfo

  case snd <$> propInfo of
    Nothing -> throwError [MCInternalPropInfoError actIdent]
    Just info -> return info
{-# INLINE actionInfoView #-}

newtype BaseT :: Context -> (Type -> Type) -> Type -> Type where
  BaseT :: {runBaseT :: ReaderT MCEnv (StateT MCState (ExceptT [MCError ctxt] m)) a} -> BaseT ctxt m a
  deriving (Functor, Applicative, Monad)
  deriving (MonadReader MCEnv, MonadState MCState, MonadIO)

-- | @since 0.1.0.0
deriving instance
  Monad m => MonadError [MCError ctxt] (BaseT ctxt m)

-- | @since 0.1.0.0
instance MonadTrans (BaseT ctxt) where
  lift = BaseT . lift . lift . lift
  {-# INLINE lift #-}
