module Language.Spectacle
  ( -- * Model Checking
    defaultInteraction,

    -- * Syntax
    type Action,
    type Invariant,
    type Terminate,

    -- ** Variables
    plain,
    prime,
    type (#),

    -- ** Modal Operators
    always,
    eventually,
    upUntil,

    -- ** Operators
    (.=),
    enabled,
    throwE,
    catchE,

    -- ** Logic
    forall,
    exists,
    oneOf,
    conjunct,
    (/\),
    disjunct,
    (\/),
    complement,
    (==>),
    implies,
    (<=>),
    iff,
  )
where

import Data.Type.Rec (type (#))
import Language.Spectacle.AST
  ( Action,
    Invariant,
    Terminate,
  )
import Language.Spectacle.Checker
import Language.Spectacle.Interaction (defaultInteraction)
import Language.Spectacle.Syntax
  ( always,
    catchE,
    complement,
    conjunct,
    disjunct,
    enabled,
    eventually,
    exists,
    forall,
    iff,
    implies,
    oneOf,
    plain,
    prime,
    throwE,
    upUntil,
    (.=),
    (/\),
    (<=>),
    (==>),
    (\/),
  )

-- ---------------------------------------------------------------------------------------------------------------------
