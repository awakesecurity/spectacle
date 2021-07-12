{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Language.Spectacle.Checker.Model.Internal
  ( Model (Model),
    runModel,
  )
where

import Control.Applicative (Alternative)
import Control.Monad.Except (ExceptT, MonadError, runExceptT)
import Control.Monad.Reader (MonadReader, ReaderT (runReaderT))
import Control.Monad.State.Strict (MonadState, State, runState)
import Control.Monad.Writer.Strict (MonadWriter)
import Data.Function ((&))

import Control.Monad.Heap (HeapT (HeapT), searchHeapT)
import Language.Spectacle.Checker.Fingerprint (Fingerprint)
import Language.Spectacle.Checker.Model.MCError (MCError)
import Language.Spectacle.Checker.Model.ModelEnv (ModelEnv)
import Language.Spectacle.Checker.Universe (Universe)

-- ---------------------------------------------------------------------------------------------------------------------

newtype Model ctx a where
  Model ::
    HeapT [Fingerprint] (ExceptT [MCError ctx] (ReaderT (ModelEnv ctx) (State Universe))) a ->
    Model ctx a
  deriving (Functor, Applicative, Monad, Alternative)
  deriving
    ( MonadReader (ModelEnv ctx)
    , MonadState Universe
    , MonadError [MCError ctx]
    , MonadWriter [Fingerprint]
    )

runModel :: ModelEnv ctx -> Universe -> Model ctx a -> (Either [MCError ctx] [(a, [Fingerprint])], Universe)
runModel modelEnv universe (Model model) =
  model
    & searchHeapT
    & runExceptT
    & flip runReaderT modelEnv
    & flip runState universe
