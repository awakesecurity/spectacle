module Language.Spectacle
  ( -- * CLI Interaction
    interaction,

    -- * Model Checking
    modelcheck,
    modeltrace,

    -- * Syntax
    type Action,

    -- ** Variables
    plain,
    prime,
    type (#),

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
  )
import Language.Spectacle.Interaction (interaction)
import Language.Spectacle.Model (modelcheck, modeltrace)
import Language.Spectacle.Syntax
  ( catchE,
    complement,
    conjunct,
    disjunct,
    enabled,
    exists,
    forall,
    iff,
    implies,
    oneOf,
    plain,
    prime,
    throwE,
    (.=),
    (/\),
    (<=>),
    (==>),
    (\/),
  )

-- ---------------------------------------------------------------------------------------------------------------------
