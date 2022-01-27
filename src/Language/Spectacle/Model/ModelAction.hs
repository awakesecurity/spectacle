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
import Data.Hashable (Hashable)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Control.Monad.IO.Class
import Lens.Micro ((^.))
import Lens.Micro.Extras (view)

import Data.Type.Rec
import Data.World
import Language.Spectacle.AST
import Language.Spectacle.Exception.RuntimeException
import Language.Spectacle.AST.Action (runExceptionalAction)
import Language.Spectacle.Model.Monad
import Language.Spectacle.Model.ModelError (ModelError)
import qualified Language.Spectacle.Model.ModelError as ModelError
import Language.Spectacle.Specification

-- ---------------------------------------------------------------------------------------------------------------------

data ModelAction ctx = ModelAction
  { modelActionName :: {-# UNPACK #-} !String
  , getModelAction :: World ctx -> Either RuntimeException (Set (World ctx))
  }

runModelAction ::
  (MonadIO m, HasDict Hashable ctx) =>
  World ctx ->
  ModelAction ctx ->
  ModelM m (Set (World ctx))
runModelAction world action =
  case getModelAction action world of
    Left err -> throwError (RuntimeError err)
    Right worlds
      | Set.null worlds -> pure (Set.singleton world)
      | otherwise -> pure worlds

fromActionSpec :: HasDict Hashable ctx => Map String (Action ctx Bool) -> [ModelAction ctx]
fromActionSpec = Map.foldrWithKey (\k v xs -> go k v : xs) []
  where
    go name action = ModelAction name ((`runExceptionalAction` action) . view worldValues)
