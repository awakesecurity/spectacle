module Language.Spectacle.RTS
  ( -- * Spectacle Exceptions
    RuntimeError,
    RuntimeException,
    throwError,
    runError,

    -- ** Variable Exceptions
    VariableException,
    throwCyclicReference,

    -- ** Quantifier Exceptions
    QuantifierException,
    throwForallViolation,
    throwEmptyExists,
    throwExistsViolation,

    -- * Call Stacks
    Callstack (Callstack),
    pushCall,
    isCircular,
    prettyCallstack,
  )
where

import Language.Spectacle.RTS.Callstack
  ( Callstack (Callstack),
    isCircular,
    prettyCallstack,
    pushCall,
  )
import Language.Spectacle.RTS.Exception
  ( QuantifierException,
    RuntimeError,
    RuntimeException,
    VariableException,
    runError,
    throwCyclicReference,
    throwEmptyExists,
    throwError,
    throwExistsViolation,
    throwForallViolation,
  )

-- -----------------------------------------------------------------------------
