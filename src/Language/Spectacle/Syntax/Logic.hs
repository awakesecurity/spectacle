-- | Quantifiers and logic.
--
-- @since 0.1.0.0
module Language.Spectacle.Syntax.Logic
  ( Logic (Logic),
    Effect (Forall, Exists, Complement, Conjunct, Disjunct),
    forall,
    exists,
    complement,
    conjunct,
    disjunct,
    implies,
    iff,
    runLogic,
    rewriteComplement,
  )
where

import Control.Applicative (Alternative ((<|>)), Applicative (liftA2))
import Control.Monad (unless)
import Data.Coerce (coerce)
import Data.Foldable (toList)
import Data.Void (absurd)

import Data.Functor.Loom (hoist, runLoom, (~>~))
import Language.Spectacle.Exception.RuntimeException
  ( QuantifierException (ExistsViolated, ForallViolated),
    RuntimeException (QuantifierException),
  )
import Language.Spectacle.Lang
  ( Lang (Op, Pure, Scoped),
    Member (projectS),
    Members,
    decomposeOp,
    decomposeS,
    scope,
  )
import Language.Spectacle.Syntax.Error (Error, throwE)
import Language.Spectacle.Syntax.Logic.Internal
  ( Effect (..),
    Logic (Logic),
  )
import Language.Spectacle.Syntax.NonDet (NonDet)

-- -------------------------------------------------------------------------------------------------

-- | Universally quantify over some foldable container @f a@. A nondeterministically chosen element
-- in @f a@ will be returned so long as the given predicate is 'True' for all elements in the
-- container, otherwise a spectacle exception is raised.
--
-- @since 0.1.0.0
forall ::
  (Members '[Logic, Error RuntimeException, NonDet] effs, Foldable f) =>
  f a ->
  (a -> Lang ctx effs Bool) ->
  Lang ctx effs Bool
forall xs p = scope (Forall (toList xs) p)
{-# INLINE forall #-}

-- | Existential quantification over some foldable constainer @f a@. A nondeterministically chosen
-- element in @f a@ which satisfies the given predicate will be returned. If there exists no element
-- in the container that satisfies the predicate then an exception is raised.
--
-- @since 0.1.0.0
exists ::
  (Members '[Logic, Error RuntimeException, NonDet] effs, Foldable f) =>
  f a ->
  (a -> Lang ctx effs Bool) ->
  Lang ctx effs Bool
exists xs p = scope (Exists (toList xs) p)
{-# INLINE exists #-}

-- | Logical negation. The 'complement' operator is equivalent to 'not' for simple expressions, but
-- can be used to negate quantifiers and the other logical operators in spectacle.
--
-- @since 0.1.0.0
complement :: Members Logic effs => Lang ctx effs Bool -> Lang ctx effs Bool
complement m = scope (Complement m)

-- | Boolean conjunction.
--
-- @since 0.1.0.0
conjunct :: Members '[Logic, NonDet] effs => Lang ctx effs Bool -> Lang ctx effs Bool -> Lang ctx effs Bool
conjunct m n = scope (Conjunct m n)

-- | Boolean disjunction.
--
-- @since 0.1.0.0
disjunct :: Members '[Logic, NonDet] effs => Lang ctx effs Bool -> Lang ctx effs Bool -> Lang ctx effs Bool
disjunct m n = scope (Disjunct m n)

-- | Logical implication.
--
-- @since 0.1.0.0
implies :: Members '[Logic, NonDet] effs => Lang ctx effs Bool -> Lang ctx effs Bool -> Lang ctx effs Bool
implies m n = complement (conjunct m (complement n))

-- | If and only if.
--
-- @since 0.1.0.0
iff :: Members '[Logic, NonDet] effs => Lang ctx effs Bool -> Lang ctx effs Bool -> Lang ctx effs Bool
iff m n = conjunct (implies m n) (implies n m)

-- | Discharge a 'Logic' effect.
--
-- @since 0.1.0.0
runLogic ::
  Members '[Error RuntimeException, NonDet] effs =>
  Lang ctx (Logic ': effs) a ->
  Lang ctx effs a
runLogic = \case
  Pure x -> pure x
  Op op k -> case decomposeOp op of
    Left other -> Op other (runLogic . k)
    Right bottom -> absurd (coerce bottom)
  Scoped scoped loom -> case decomposeS scoped of
    Left other -> Scoped other loom'
    Right (Forall xs p) -> runLoom loom' do
      b <- fmap and (mapM p xs)
      unless b (throwE (QuantifierException ForallViolated))
      return b
    Right (Exists xs p) -> runLoom loom' do
      b <- fmap or (mapM p xs)
      unless b (throwE (QuantifierException ExistsViolated))
      return b
    Right (Complement m) -> runLoom loom' (rewriteComplement m)
    Right (Conjunct m n) -> runLoom loom' (liftA2 (&&) m n)
    Right (Disjunct m n) -> runLoom loom' (m <|> n)
    where
      loom' = loom ~>~ hoist runLogic

-- | Applies negation to logical operators.
--
-- @since 0.1.0.0
rewriteComplement :: Members Logic effs => Lang ctx effs a -> Lang ctx effs a
rewriteComplement = \case
  Pure x -> pure x
  Op op k -> Op op (rewriteComplement . k)
  Scoped scoped loom -> case projectS scoped of
    Nothing -> Scoped scoped loom'
    Just (Forall xs p) -> runLoom loom (exists xs (fmap not . p))
    Just (Exists xs p) -> runLoom loom (forall xs (fmap not . p))
    Just (Complement m) -> runLoom loom' m
    Just (Conjunct m n) -> do
      let m' = complement m
          n' = complement n
      runLoom loom' (disjunct m' n')
    Just (Disjunct m n) -> do
      let m' = complement m
          n' = complement n
      runLoom loom' (scope (Conjunct m' n'))
    where
      loom' = loom ~>~ hoist rewriteComplement
