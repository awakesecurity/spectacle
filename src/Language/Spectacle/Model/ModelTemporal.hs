-- |
--
-- @since 1.0.0
module Language.Spectacle.Model.ModelTemporal
  ( -- * Model Temporal Formulae
    ModelTemporal (ModelTemporal),
    modelTemporalName,
    getModelTemporal,

    -- ** Construction
    fromTemporalSpec,
  )
where

import Data.Map (Map)
import qualified Data.Map as Map
import Lens.Micro ((^.))

import Data.World (World, worldValues)
import Language.Spectacle.AST.Temporal (Temporal, runTemporal)

-- ---------------------------------------------------------------------------------------------------------------------

data ModelTemporal ctx = ModelTemporal
  { modelTemporalName :: String
  , getModelTemporal :: World ctx -> World ctx -> Bool
  }

fromTemporalSpec :: Map String (Temporal ctx Bool) -> [ModelTemporal ctx]
fromTemporalSpec = Map.foldrWithKey (\k v xs -> go k v : xs) []
  where
    go name formula =
      let runner w0 w1 = runTemporal (w0 ^. worldValues) (w1 ^. worldValues) formula
       in ModelTemporal name runner
