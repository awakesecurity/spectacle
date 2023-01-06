-- |
-- Module      :  Language.Spectacle.Syntax
-- Copyright   :  (c) Arista Networks, 2022-2023
-- License     :  Apache License 2.0, see LICENSE
--
-- Stability   :  stable
-- Portability :  non-portable (GHC extensions)
--
-- TODO: docs
--
-- @since 1.0.0
module Language.Spectacle
  ( -- * CLI Interaction
    interaction,

    -- * Model Checking
    modelcheck,
    modeltrace,

    -- * Specification
    Specification (Specification),
    specInit,
    specNext,
    specProp,
    ActionType (ActionSF, ActionWF, ActionUF),
    TemporalType (PropF, PropG, PropGF, PropFG),
    Fairness (StrongFair, WeakFair, Unfair),
    Modality (Always, Eventually, Infinitely, Stays),

    -- * Syntax
    type Action,
    type Temporal,

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

    -- * Records
    pattern ConF,
    pattern NilF,
  )
where

import Data.Type.Rec (RecF (ConF, NilF), type (#))
import Language.Spectacle.AST (Action, Temporal)
import Language.Spectacle.Fairness (Fairness (StrongFair, Unfair, WeakFair))
import Language.Spectacle.Interaction (interaction)
import Language.Spectacle.Model (modelcheck, modeltrace)
import Language.Spectacle.Specification
  ( ActionType (ActionSF, ActionUF, ActionWF),
    Modality (Always, Eventually, Infinitely, Stays),
    Specification (Specification),
    TemporalType (PropF, PropFG, PropG, PropGF),
    specInit,
    specNext,
    specProp,
  )
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
