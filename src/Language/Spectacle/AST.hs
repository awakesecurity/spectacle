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

-- ---------------------------------------------------------------------------------------------------------------------
