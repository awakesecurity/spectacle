module Language.Spectacle.AST
  ( -- * Initial Actions
    type Initial,
    type InitialSyntax,

    -- ** Interpreters
    runInitial,

    -- * Temporal Actions
    type Action,
    type ActionSyntax,

    -- ** Interpreters
    runAction,

    -- * Invariants
    type Invariant,
    type InvariantSyntax,

    -- ** Interpreters
    runInvariant,

    -- ** Rewriting
    applyRewrites,

    -- * Termination
    type Terminate,
    type TerminateSyntax,

    -- ** Interpreters
    runTerminate,
  )
where

import Language.Spectacle.AST.Action (Action, ActionSyntax, runAction)
import Language.Spectacle.AST.Initial (Initial, InitialSyntax, runInitial)
import Language.Spectacle.AST.Invariant (Invariant, InvariantSyntax, applyRewrites, runInvariant)
import Language.Spectacle.AST.Terminate (Terminate, TerminateSyntax, runTerminate)

-- ---------------------------------------------------------------------------------------------------------------------
