module Language.Spectacle.AST
  (-- * Temporal Actions
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

import Language.Spectacle.AST.Action
import Language.Spectacle.AST.Invariant (Invariant, InvariantSyntax, applyRewrites, runInvariant)
import Language.Spectacle.AST.Terminate (Terminate, TerminateSyntax, runTerminate)

-- ---------------------------------------------------------------------------------------------------------------------
