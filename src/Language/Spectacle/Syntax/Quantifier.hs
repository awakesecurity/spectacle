module Language.Spectacle.Syntax.Quantifier
  ( Quantifier (..),
    exists,
    forall,
    runQuantifier,
  )
where

import Control.Monad (filterM)
import qualified GHC.Stack as GHC

import Language.Spectacle.Effect.NonDet (NonDet, foldMapA, oneOf)
import Language.Spectacle.Lang
  ( Lang,
    Syntax,
    interpretWith,
    send,
    type (|>),
  )
import Language.Spectacle.RTS.Exception
  ( RuntimeError,
    throwEmptyExists,
    throwExistsViolation,
    throwForallViolation,
  )

-- -----------------------------------------------------------------------------

data Quantifier :: Syntax where
  Exists :: Show a => GHC.CallStack -> [a] -> (a -> m Bool) -> Quantifier m a
  Forall :: Show a => GHC.CallStack -> [a] -> (a -> m Bool) -> Quantifier m a

exists ::
  (GHC.HasCallStack, Show a, Quantifier |> sig) =>
  [a] ->
  (a -> Lang sig cxt Bool) ->
  Lang sig cxt a
exists domain predicate = send (Exists GHC.callStack domain predicate)
{-# INLINE exists #-}

forall ::
  (GHC.HasCallStack, Show a, Quantifier |> sig) =>
  [a] ->
  (a -> Lang sig cxt Bool) ->
  Lang sig cxt a
forall domain predicate = send (Forall GHC.callStack domain predicate)
{-# INLINE forall #-}

runQuantifier ::
  (NonDet |> sig, RuntimeError |> sig) =>
  Lang (Quantifier ': sig) cxt a ->
  Lang sig cxt a
runQuantifier = interpretWith \_ quantifier k -> case quantifier of
  Exists ghcStack [] _ -> throwEmptyExists ghcStack
  Exists ghcStack domain predicate -> do
    xs <- filterM predicate domain
    if null xs
      then throwExistsViolation ghcStack
      else oneOf xs >>= k
  Forall ghcStack domain predicate ->
    -- The way 'forall' is interpreted here, vacuous truth can be assumed for
    -- any domain where membership is false. Universal quantification over an
    -- empty domain, i.e. the domain is the empty set. In this case, a vacuous
    -- 'forall' would simply propagate @empty :: [a]@ up to the model checker
    -- where its handled either by witnessing that model checking cannot
    -- continue from the current program trace and throwing whatever the
    -- equivalent of the TLA+ deadlock diagnostic is, or just accepting the
    -- program assuming some termination condition (temporal operator
    -- "eventually") has been met.
    --
    -- Vacuity should still hold when 'forall' is used in the definition of a
    -- program invariant. For example, a program trace which is equal to the
    -- empty set or in other words, a program which fails regardless of its
    -- inputs and produces no states, would satisfy 'forall'. Spectacle should
    -- absolutely have a vacuity checker because of this.
    --
    -- In the future, there should be syntax for bringing the domain - or at the
    -- very least the cardinality of the domain - into scope so that users can
    -- guard against or explicitly accept vacuous antecedents.
    flip foldMapA domain \x -> do
      doesSatisfy <- predicate x
      if doesSatisfy
        then k x
        else throwForallViolation ghcStack
{-# INLINE runQuantifier #-}
