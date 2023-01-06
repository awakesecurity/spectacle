-- |
-- Module      :  Language.Spectacle.Model.ModelAction
-- Copyright   :  (c) Arista Networks, 2022-2023
-- License     :  Apache License 2.0, see LICENSE
--
-- Stability   :  stable
-- Portability :  non-portable (GHC extensions)
--
-- TODO: docs
--
-- @since 1.0.0
module Language.Spectacle.Model.ModelAction
  ( -- * Model Action
    ModelAction (ModelAction),
    modelActionName,
    getModelAction,

    -- ** Construction
    fromActionSpec,

    -- ** Deconstruction
    runModelAction,
  )
where

import Control.Monad.Except (MonadError, throwError)
import Data.Hashable (Hashable)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import Lens.Micro ((^.))

import Data.Type.Rec (HasDict)
import Data.World (World, worldValues)
import Language.Spectacle.AST (Action)
import Language.Spectacle.AST.Action (runExceptionalAction)
import Language.Spectacle.Exception.RuntimeException (RuntimeException)
import Language.Spectacle.Model.ModelError
  ( ModelError (RuntimeError),
  )

-- ---------------------------------------------------------------------------------------------------------------------

data ModelAction ctx = ModelAction
  { modelActionName :: String
  , getModelAction :: World ctx -> Either RuntimeException (Set (World ctx))
  }

runModelAction :: MonadError (ModelError ctx) m => World ctx -> ModelAction ctx -> m (Set (World ctx))
runModelAction world action =
  case getModelAction action world of
    Left err -> throwError (RuntimeError err)
    Right worlds -> pure worlds

fromActionSpec :: (HasDict Eq ctx, HasDict Hashable ctx) => Map String (Action ctx Bool) -> [ModelAction ctx]
fromActionSpec =
  Map.foldMapWithKey \name action ->
    let runner world = runExceptionalAction (world ^. worldValues) action
     in [ModelAction name runner]
