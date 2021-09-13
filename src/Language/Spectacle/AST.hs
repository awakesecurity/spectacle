module Language.Spectacle.AST
  ( -- * Temporal Actions
    type Action,
    type ActionSyntax,

    -- ** Interpreters
    runAction,

    -- * Termination
    type Terminate,
    type TerminateSyntax,

    -- ** Interpreters
    runTerminate,
  )
where

import Language.Spectacle.AST.Action (Action, ActionSyntax, runAction)
import Language.Spectacle.AST.Terminate (Terminate, TerminateSyntax, runTerminate)

-- ---------------------------------------------------------------------------------------------------------------------
