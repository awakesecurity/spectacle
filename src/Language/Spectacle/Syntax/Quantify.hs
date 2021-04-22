-- | Universal and existential quantifiers.
--
-- @since 0.1.0.0
module Language.Spectacle.Syntax.Quantify
  ( Quantify (Quantify),
    Effect (Forall, Exists),
    forall,
    exists,
    runQuantify,
  )
where

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
    Members,
    Union (Op, Scoped),
    decomposeOp,
    decomposeS,
    scope,
  )
import Language.Spectacle.Syntax.Error (Error, throwE)
import Language.Spectacle.Syntax.NonDet (NonDet, oneOf)
import Language.Spectacle.Syntax.Quantify.Internal (Effect (Exists, Forall), Quantify (Quantify))

-- -------------------------------------------------------------------------------------------------

-- | Universally quantify over some foldable container @f a@. A nondeterministically chosen element
-- in @f a@ will be return so long as the given predicate is 'True' for all elements in the
-- container, otherwise an exception is raised.
--
-- @since 0.1.0.0
forall ::
  (Members '[Quantify, Error RuntimeException, NonDet] effs, Foldable f) =>
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
  (Members '[Quantify, Error RuntimeException, NonDet] effs, Foldable f) =>
  f a ->
  (a -> Lang ctx effs Bool) ->
  Lang ctx effs a
exists xs p = scope (Exists (foldMap (: []) xs) p)
{-# INLINE exists #-}

-- | Discharge a 'Quantify' effect.
--
-- @since 0.1.0.0
runQuantify ::
  Members '[Error RuntimeException, NonDet] effs =>
  Lang ctx (Quantify ': effs) a ->
  Lang ctx effs a
runQuantify = \case
  Pure x -> Pure x
  Yield (Op op) k -> case decomposeOp op of
    Left other -> Yield (Op other) (runQuantify . k)
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
    where
      loom' = loom ~>~ hoist runQuantify

      k' = runQuantify . k
