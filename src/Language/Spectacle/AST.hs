-- |
-- Module      :  Language.Spectacle.AST
-- Copyright   :  (c) Arista Networks, 2022-2023
-- License     :  Apache License 2.0, see LICENSE
--
-- Stability   :  stable
-- Portability :  non-portable (GHC extensions)
--
-- TODO: docs
--
-- @since 1.0.0
module Language.Spectacle.AST
  ( -- * Actions
    type Action,
    runAction,

    -- * Relations
    type Temporal,
    runTemporal,
  )
where

import Language.Spectacle.AST.Action (Action, runAction)
import Language.Spectacle.AST.Temporal (Temporal, runTemporal)