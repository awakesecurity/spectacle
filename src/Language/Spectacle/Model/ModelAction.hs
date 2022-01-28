-- |
--
-- @since 0.1.0.0
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
import Control.Monad.IO.Class
import Data.Hashable (Hashable)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Lens.Micro ((^.))
import Lens.Micro.Extras (view)

import Data.Type.Rec
import Data.World
import Language.Spectacle.AST
import Language.Spectacle.AST.Action (runExceptionalAction)
import Language.Spectacle.Exception.RuntimeException
import Language.Spectacle.Model.ModelError (ModelError)
import qualified Language.Spectacle.Model.ModelError as ModelError
import Language.Spectacle.Model.Monad
import Language.Spectacle.Specification

-- ---------------------------------------------------------------------------------------------------------------------

data ModelAction ctx = ModelAction
  { modelActionName :: String
  , getModelAction :: World ctx -> Either RuntimeException (Set (World ctx))
  }

runModelAction :: MonadError ModelError m => World ctx -> ModelAction ctx -> m (Set (World ctx))
runModelAction world action =
  case getModelAction action world of
    Left err -> throwError (RuntimeError err)
    Right worlds -> pure worlds

fromActionSpec :: HasDict Hashable ctx => Map String (Action ctx Bool) -> [ModelAction ctx]
fromActionSpec =
  Map.foldMapWithKey \name action ->
    let runner world = runExceptionalAction (world ^. worldValues) action
     in [ModelAction name runner]
