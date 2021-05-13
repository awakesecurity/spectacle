module Language.Spectacle.Syntax.Quantifier
  ( Quantifier (Quantifier),
    Effect (Forall, Exists),
    forall,
    exists,
    runQuantifier,
  )
where

import Control.Applicative (Alternative (empty))
import Control.Monad (unless)
import Data.Bool (bool)
import Data.Coerce (coerce)
import Data.Foldable (Foldable (toList))
import Data.Void (absurd)

import Data.Functor.Loom (hoist, runLoom, (~>~))
import Language.Spectacle.Exception.RuntimeException
  ( QuantifierException (ExistsViolated, ForallViolated),
    RuntimeException (QuantifierException),
  )
import Language.Spectacle.Lang
  ( Effect,
    Lang (..),
    Member,
    Members,
    decomposeOp,
    decomposeS,
    scope,
  )
import Language.Spectacle.Syntax.Error (Error, throwE)
import Language.Spectacle.Syntax.NonDet (NonDet, foldMapA, msplit, oneOf)
import Language.Spectacle.Syntax.Quantifier.Internal (Effect (Exists, Forall), Quantifier (Quantifier))

-- ---------------------------------------------------------------------------------------------------------------------

-- | Universally quantify over some foldable container @f a@. A nondeterministically chosen element in @f a@ will be
-- returned so long as the given predicate is 'True' for all elements in the container, otherwise a spectacle exception
-- is raised.
--
-- @since 0.1.0.0
forall :: (Foldable f, Member Quantifier effs) => f a -> (a -> Lang ctx effs Bool) -> Lang ctx effs Bool
forall xs p = scope (Forall (toList xs) p)
{-# INLINE forall #-}

-- | Existential quantification over some foldable constainer @f a@. A nondeterministically chosen element in @f a@
-- which satisfies the given predicate will be returned. If there exists no element in the container that satisfies the
-- predicate then an exception is raised.
--
-- @since 0.1.0.0
exists :: (Foldable f, Member Quantifier effs) => f a -> (a -> Lang ctx effs Bool) -> Lang ctx effs Bool
exists xs p = scope (Exists (toList xs) p)
{-# INLINE exists #-}

runQuantifier ::
  Members '[Error RuntimeException, NonDet] effs =>
  Lang ctx (Quantifier ': effs) Bool ->
  Lang ctx effs Bool
runQuantifier = \case
  Pure x -> pure x
  Op op k -> case decomposeOp op of
    Left other -> Op other (runQuantifier . k)
    Right bottom -> absurd (coerce bottom)
  Scoped scoped loom -> case decomposeS scoped of
    Left other -> Scoped other loom'
    Right (Forall xs p) -> do
      b <- oneOf xs >>= runLoom loom' . p
      unless b (throwE (QuantifierException ForallViolated))
      return b
    Right (Exists [] _) -> throwE (QuantifierException ExistsViolated)
    Right (Exists dom p) -> do
      let m' = flip foldMapA dom \x -> runLoom loom' (p x) >>= \b -> bool empty (pure b) b
      msplit m' >>= \case
        Just _ -> m'
        Nothing -> throwE (QuantifierException ExistsViolated)
    where
      loom' = loom ~>~ hoist runQuantifier
{-# INLINE runQuantifier #-}
