{-# LANGUAGE EmptyCase #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_HADDOCK show-extensions #-}

-- |
-- Module      :  Language.Spectacle.Lang.Scoped
-- Copyright   :  (c) Arista Networks, 2022-2023
-- License     :  Apache License 2.0, see LICENSE
--
-- Stability   :  stable
-- Portability :  non-portable (GHC extensions)
--
-- Defining and handling higher-order effects in the 'Effect' data family.
--
-- === Example usage of the Effect family
--
-- The ask operation for the Reader effect is given by GADT:
--
-- @
-- data Reader r :: 'EffectK' where
--   Ask :: Reader r r
-- @
--
-- 'EffectK' intentionally precludes the ability for accessing the 
-- 'Language.Spectacle.Lang.Lang' in its continuation which is needed to define
-- a "local" operation for Reader. If this was allowed, it becomes very easy for
-- effects to escape the scope of the free monad and continue without being 
-- handled, examples of which are in [1. Effect Handlers in Haskell, Evidently.]("Language.Spectacle.Lang.Scoped#references").
-- Instead, scoped operations like local are defined in a corresponding instance
-- of 'Effect' like so:
--
-- @
-- data instance 'Effect' (Reader r) m a where
--   Local :: m a -> (r -> r) -> 'Effect' (Reader r) m a
-- @
--
-- Where @m@ is some 'Langauge.Spectacle.Lang.Lang'. This approach lets us define scoped operations
-- which may have:
--
-- * Dependencies on other effects.
-- * Constraints on types in the effect.
-- * Monomorphic types in the continuation.
--
-- Effects which only have first-order operations must still give a newtype instance for 'Effect'
-- wrapping 'Data.Void.Void' and handled with 'Data.Void.absurd'.
--
-- === Reasoning behind the 'Effect' family
--
-- The typical approach to handling higher-order effects is done by "weaving" which is described in
-- [2. Effect Handler in Scope]("Language.Spectacle.Lang.Scoped#references"). It requires that every
-- effect have an instance of @(forall x. f (m x) -> n (f x))@. This forces all effects to be
-- polymorphic in their continuation due to the rigidity of @x@. Effects like the quantifier syntax
-- can't be defined this way since there is no way to weave @(a -> m Bool)@.
--
-- An alternative would be to change the continuation of the Freer monad to accumulate weaves rather
-- than monadic actions, thus pushing the weaving responsibility to the handler site. This allows
-- monomorphic effects and constrained data contexts, but struggles with handling effects like NonDet
-- since it's not possible to distribute functors over the freer monad. The way to work around this is
-- to use a concrete functorial state like ListT to distribute and then fold back into the resulting
-- alternative. This works at the consequence of having different semantics for NonDet in first-
-- order vs higher-order operations.
--
-- The 'Effect' family solves boths problems since:
--
-- 1. Weaving 'Effect' instances is done in the handler for the effect.
--
-- 2. An 'Effect' instance is a seperate case from its corresponding first-order effect so effects
-- like NonDet can weave in terms of its own interpreter rather than choosing a concrete functor as
-- an intermediate carrier.
--
-- #references#
--
-- === Reference
--
-- 1. [Effect Handlers in Haskell, Evidently](https://xnning.github.io/papers/haskell-evidently.pdf)
-- 2. [Effect Handlers in Scope](https://www.cs.ox.ac.uk/people/nicolas.wu/papers/Scope.pdf)
--
-- @since 1.0.0
module Language.Spectacle.Lang.Scoped
  ( -- * Effect Kinds
    EffectK,
    ScopeK,

    -- * Effect family
    FirstOrder,
    Effect,

    -- * Higher-order union
    Scoped (SHere, SThere),
    decomposeS,
    extractS,
  )
where

import Data.Coerce (Coercible)
import Data.Kind (Constraint, Type)
import Data.Void (Void)

-- -------------------------------------------------------------------------------------------------

-- | The kind of first-order effects.
--
-- @since 1.0.0
type EffectK = Type -> Type

-- | The kind of higher-order effects.
--
-- @since 1.0.0
type ScopeK = (Type -> Type) -> Type -> Type

-- | Constraint for first-order effects.
--
-- @since 1.0.0
type FirstOrder :: EffectK -> Constraint

type FirstOrder eff = forall m a. Coercible (Effect eff m a) Void

-- | 'Effect' is a family of higher-order operations for the effect @eff@.
--
-- @since 1.0.0
type Effect :: EffectK -> ScopeK
data family Effect eff m a

-- -------------------------------------------------------------------------------------------------

-- | 'Scoped' is an extensible sum inhabited by the higher-order operations of some effect in
-- @effs@.
--
-- @since 1.0.0
data Scoped effs m a where
  SHere :: Effect eff m a -> Scoped (eff ': effs) m a
  SThere :: Scoped effs m a -> Scoped (eff ': effs) m a

-- | Orthogonal decomposition of a 'Scoped'. Decomposing a 'Scoped' can yield either the 'Effect'
-- instance for @eff@ or witness a proof that @Eff eff m a@ does not inhabit this sum and remove
-- it from the effect signature.
--
-- @since 1.0.0
decomposeS :: Scoped (eff ': effs) m a -> Either (Scoped effs m a) (Effect eff m a)
decomposeS (SHere eff) = Right eff
decomposeS (SThere s) = Left s
{-# INLINE decomposeS #-}

-- | A special case of 'decomposeS'. A singleton sum of @eff@ must be inhabited by @eff@.
--
-- @since 1.0.0
extractS :: Scoped '[eff] m a -> Effect eff m a
extractS (SHere eff) = eff
extractS (SThere s) = case s of
