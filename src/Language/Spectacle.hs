module Language.Spectacle
  ( -- * Model Checking
    doModelCheck,

    -- * Syntax
    type Initial,
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
    define,
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
    Initial,
    Invariant,
    Terminate,
  )
import Language.Spectacle.Spec (doModelCheck)
import Language.Spectacle.Syntax
  ( always,
    catchE,
    complement,
    conjunct,
    define,
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
