-- | Quantifiers and logic.
--
-- @since 0.1.0.0
module Language.Spectacle.Syntax.Logic
  ( Logic (Logic),
    Effect (Forall, Exists),
    forall,
    exists,
    complement,
    conjunct,
    disjunct,
    implies,
    iff,
    runLogic,
  )
where

import Control.Applicative (Applicative (liftA2))
import Control.Monad (filterM, unless, when)
import Data.Coerce (coerce)
import Data.Void (absurd)

import Data.Functor.Loom (hoist, runLoom, (~>~))
import Language.Spectacle.Exception.RuntimeException
  ( QuantifierException (ExistsViolated, ForallViolated),
    RuntimeException (QuantifierException),
  )
import Language.Spectacle.Lang
  ( Effect,
    Lang (Pure, Yield),
    Member (projectS),
    Members,
    Union (Op, Scoped),
    decomposeOp,
    decomposeS,
    scope,
  )
import Language.Spectacle.Syntax.Error (Error, throwE)
import Language.Spectacle.Syntax.Logic.Internal
  ( Effect (Complement, Conjunct, Disjunct, Exists, Forall),
    Logic (Logic),
  )
import Language.Spectacle.Syntax.NonDet (NonDet, oneOf)

-- -------------------------------------------------------------------------------------------------

-- | Universally quantify over some foldable container @f a@. A nondeterministically chosen element
-- in @f a@ will be return so long as the given predicate is 'True' for all elements in the
-- container, otherwise an exception is raised.
--
-- @since 0.1.0.0
forall ::
  (Members '[Logic, Error RuntimeException, NonDet] effs, Foldable f) =>
  f a ->
  (a -> Lang ctx effs Bool) ->
  Lang ctx effs a
forall xs p = scope (Forall (foldMap (: []) xs) p)
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
  Lang ctx effs a
exists xs p = scope (Exists (foldMap (: []) xs) p)
{-# INLINE exists #-}

-- | Logical negation. The 'complement' operator is equivalent to 'not' for simple expressions, but
-- can be used to negate quantifiers and the other logical operators in spectacle.
--
-- @since 0.1.0.0
complement :: Member Logic effs => Lang ctx effs a -> Lang ctx effs a
complement m = scope (Complement m)

-- | Boolean conjunction.
--
-- @since 0.1.0.0
conjunct :: Member Logic effs => Lang ctx effs Bool -> Lang ctx effs Bool -> Lang ctx effs Bool
conjunct m n = scope (Conjunct m n)

-- | Boolean disjunction.
--
-- @since 0.1.0.0
disjunct :: Member Logic effs => Lang ctx effs Bool -> Lang ctx effs Bool -> Lang ctx effs Bool
disjunct m n = scope (Disjunct m n)

-- | Logical implication.
--
-- @since 0.1.0.0
implies :: Member Logic effs => Lang ctx effs Bool -> Lang ctx effs Bool -> Lang ctx effs Bool
implies m n = complement (conjunct m (complement n))

-- | If and only if.
--
-- @since 0.1.0.0
iff :: Member Logic effs => Lang ctx effs Bool -> Lang ctx effs Bool -> Lang ctx effs Bool
iff m n = conjunct (m `implies` n) (n `implies` m)

-- | Discharge a 'Logic' effect.
--
-- @since 0.1.0.0
runLogic ::
  Members '[Error RuntimeException, NonDet] effs =>
  Lang ctx (Logic ': effs) a ->
  Lang ctx effs a
runLogic = \case
  Pure x -> Pure x
  Yield (Op op) k -> case decomposeOp op of
    Left other -> Yield (Op other) (runLogic . k)
    Right bottom -> absurd (coerce bottom)
  Yield (Scoped scoped loom) k -> case decomposeS scoped of
    Left other -> Yield (Scoped other loom') k'
    Right (Forall xs p) ->
      k' =<< runLoom loom' do
        b <- fmap and (mapM p xs)
        unless b (throwE (QuantifierException ForallViolated))
        oneOf xs
    Right (Exists xs p) ->
      k' =<< runLoom loom' do
        xs' <- filterM p xs
        when (null xs') (throwE (QuantifierException ExistsViolated))
        oneOf xs'
    Right (Complement m) -> runLoom loom' (rewriteComplement m) >>= k'
    Right (Conjunct m n) -> runLoom loom' (liftA2 (&&) m n) >>= k'
    Right (Disjunct m n) -> runLoom loom' (liftA2 (||) m n) >>= k'
    where
      loom' = loom ~>~ hoist runLogic

      k' = runLogic . k

-- | Applies negation to logical operators.
--
-- @since 0.1.0.0
rewriteComplement :: Member Logic effs => Lang ctx effs a -> Lang ctx effs a
rewriteComplement = \case
  Pure x -> pure x
  Yield (Op op) k -> Yield (Op op) (rewriteComplement . k)
  Yield (Scoped scoped loom) k -> case projectS scoped of
    Nothing -> Yield (Scoped scoped loom') k'
    Just (Forall xs p) -> runLoom loom (exists xs (fmap not . p)) >>= k'
    Just (Exists xs p) -> runLoom loom (forall xs (fmap not . p)) >>= k'
    Just (Complement m) -> runLoom loom' m >>= k'
    Just (Conjunct m n) -> do
      let m' = complement (fmap not m)
          n' = complement (fmap not n)
      k' =<< runLoom loom' (disjunct m' n')
    Just (Disjunct m n) -> k' =<< runLoom loom' (conjunct (complement (fmap not m)) (complement (fmap not n)))
    where
      loom' = loom ~>~ hoist rewriteComplement

      k' = rewriteComplement . k
