module Language.Spectacle.RTS.Exception
  ( -- * Spectacle Exceptions
    RuntimeError (..),
    RuntimeException (..),
    throwError,
    runError,

    -- ** Variable Exceptions
    VariableException (..),
    throwCyclicReference,

    -- ** Quantifier Exceptions
    QuantifierException (..),
    throwForallViolation,
    throwEmptyExists,
    throwExistsViolation,
  )
where

import Control.Exception (Exception)
import qualified GHC.Exception as GHC

import Language.Spectacle.Lang (Lang, Syntax, handleRelay, send, type (|>))
import Language.Spectacle.RTS.Callstack (Callstack, prettyCallstack)

-- -----------------------------------------------------------------------------

-- | The type of runtime errors.
--
-- @since 0.1.0.0
newtype RuntimeError :: Syntax where
  RuntimeError :: RuntimeException -> RuntimeError m a

-- | Throw some `RuntimeException`.
--
-- @since 0.1.0.0
throwError :: RuntimeError |> sig => RuntimeException -> Lang sig cxt a
throwError err = send (RuntimeError err)
{-# INLINE throwError #-}

-- | Dispatch a `RuntimeError` returning the result in `Either` of a successful
-- computation or a `RuntimeException`.
--
-- @since 0.1.0.0
runError :: Lang (RuntimeError ': sig) cxt a -> Lang sig cxt (Either RuntimeException a)
runError = handleRelay (pure . Right) \_ (RuntimeError e) _ -> pure (Left e)
{-# INLINE runError #-}

-- | Heirarchy of runtime exceptions that can be emitted by Spectacle programs.
--
-- @since 0.1.0.0
data RuntimeException where
  VariableException :: VariableException -> RuntimeException
  QuantifierException :: QuantifierException -> RuntimeException
  deriving (Show)

-- -----------------------------------------------------------------------------

-- | The types of exceptions that can originate from the use of variables.
--
-- @since 0.1.0.0
data VariableException where
  -- | An exception thrown when the usage of primed variables forms a cycle
  -- provided Spectacle's 'Callstack' accumulated when this cycle was witnessed.
  --
  -- @since 0.1.0.0
  CyclicReference :: Callstack -> VariableException

-- | @since 0.1.0.0
instance Exception VariableException

-- | @since 0.1.0.0
instance Show VariableException where
  show = \case
    CyclicReference callstack ->
      "cycle found in the usage of primed variables:\n"
        ++ prettyCallstack callstack

-- | Emit an cyclic variable reference exception with the provided 'Callstack'.
--
-- @since 0.1.0.0
throwCyclicReference :: RuntimeError |> sig => Callstack -> Lang sig cxt a
throwCyclicReference callstack =
  throwError (VariableException (CyclicReference callstack))
{-# INLINE throwCyclicReference #-}

-- -----------------------------------------------------------------------------

-- | The types of exceptions that can originate from the use of logical
-- quantifiers.
--
-- @since 0.1.0.0
data QuantifierException where
  -- | An universal quantifier which failed for some value in the domain.
  --
  -- @since 0.1.0.0
  ForallViolation :: GHC.CallStack -> QuantifierException
  -- | An exception thrown when existential quantification is taken over an
  -- empty range of values.
  --
  -- @since 0.1.0.0
  EmptyExists :: GHC.CallStack -> QuantifierException
  -- | An existential quantifier which was not satisfied by any value in the
  -- domain.
  --
  -- @since 0.1.0.0
  ExistsViolation :: GHC.CallStack -> QuantifierException

-- | @since 0.1.0.0
instance Exception QuantifierException

-- | @since 0.1.0.0
instance Show QuantifierException where
  show = \case
    ForallViolation ghcStack ->
      "universal quantifier violated.\n" ++ GHC.prettyCallStack ghcStack
    EmptyExists ghcStack ->
      "unexpected empty domain in the usage of existential quantifier.\n"
        ++ GHC.prettyCallStack ghcStack
    ExistsViolation ghcStack ->
      "existential quantifier violated.\n" ++ GHC.prettyCallStack ghcStack

-- | Throw an exception for a violated universal quantifier.
--
-- @since 0.1.0.0
throwForallViolation :: RuntimeError |> sig => GHC.CallStack -> Lang sig cxt a
throwForallViolation ghcStack =
  throwError (QuantifierException (ForallViolation ghcStack))
{-# INLINE throwForallViolation #-}

-- | Throw an exception for an existential quantification over an empty domain
--
-- @since 0.1.0.0
throwEmptyExists :: RuntimeError |> sig => GHC.CallStack -> Lang sig cxt a
throwEmptyExists ghcStack =
  throwError (QuantifierException (EmptyExists ghcStack))

-- | Throw an exception for a violated existential quantifier.
--
-- @since 0.1.0.0
throwExistsViolation :: RuntimeError |> sig => GHC.CallStack -> Lang sig cxt a
throwExistsViolation ghcStack =
  throwError (QuantifierException (ExistsViolation ghcStack))
{-# INLINE throwExistsViolation #-}
