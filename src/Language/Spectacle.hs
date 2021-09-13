module Language.Spectacle
  ( -- * Model Checking
    defaultInteraction,

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
import Language.Spectacle.Interaction (defaultInteraction)
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
