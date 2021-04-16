{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeFamilies #-}

-- | The 'Lang' monad and functions for defining Spectacles syntax as effects.
--
-- @since 0.1.0.0
module Language.Spectacle.Lang
  ( -- * Lang
    Lang (Pure, Yield),
    runLang,
    send,
    scope,

    -- * Effects
    type EffectK,
    type ScopeK,
    FirstOrder,
    Effect,

    -- ** Interpreters

    -- *** First-order
    interpret,
    stateful,

    -- ** Membership
    type Members,
    Member (inject, project, injectS, projectS),

    -- ** Unions
    Union (Op, Scoped),
    Op (OHere, OThere),
    Scoped (SHere, SThere),
    decomposeOp,
    decomposeS,
  )
where

import Control.Natural (type (~>))
import Data.Coerce (coerce)
import Data.Void (absurd)

import Data.Functor.Loom (hoist, weave, (~>~))
import Language.Spectacle.Lang.Internal (Lang (Pure, Yield), Union (Op, Scoped), scope, send)
import Language.Spectacle.Lang.Member (Member (inject, injectS, project, projectS), type Members)
import Language.Spectacle.Lang.Op (Op (OHere, OThere), decomposeOp)
import Language.Spectacle.Lang.Scoped
  ( Effect,
    EffectK,
    FirstOrder,
    ScopeK,
    Scoped (SHere, SThere),
    decomposeS,
  )

-- -------------------------------------------------------------------------------------------------

-- | Used to unwrap the pure value in 'Lang' after all of its effects have been discharged.
--
-- @since 0.1.0.0
runLang :: Lang ctx '[] a -> a
runLang (Pure x) = x
runLang _ =
  -- @Lang ctx '[] a@ can only be constructed with 'Pure' or obtained by discharging all its
  -- effects, which would result in 'Pure'. Cases where @Lang ctx '[] a@ holds impure values mean
  -- that:
  --
  -- 1. An effect escaped the scope of 'Lang' and therefore was not discharged when the handler for
  -- that effect was run on 'Lang'. This not impossible but is /very/ difficult to do since the
  -- escaped effect would have to be hidden from 'Loom'. 'Lang' in a first-order operation,
  -- FO effects with resumptions to Lang, or intentionally weakening/coercing a @Lang ctx effs' a@
  -- into some other 'Lang' are all ways which basically guarantee that effects will be left
  -- unhandled.
  --
  -- 2. Operations like 'unsafeCoerce' were used to change the effect signature of 'Lang'.
  error
    "internal error: Lang match against Yield, this means that an effect escaped the scope of Lang \
    \and was left unhandled. This should be impossible."

-- | Interpreter combinator for purely first-order effects.
--
-- @since 0.1.0.0
interpret ::
  FirstOrder eff =>
  (Lang ctx (eff ': effs) ~> Lang ctx effs) ->
  (forall x. eff x -> (x -> Lang ctx effs a) -> Lang ctx effs a) ->
  Lang ctx (eff ': effs) a ->
  Lang ctx effs a
interpret eta handling = loop
  where
    loop = \case
      Pure x -> pure x
      Yield (Op op) k -> case decomposeOp op of
        Left other -> Yield (Op other) k'
        Right eff -> handling eff k'
        where
          k' = loop . k
      Yield (Scoped s l) k -> case decomposeS s of
        Left other -> Yield (Scoped other (l ~>~ hoist eta)) (loop . k)
        Right bot -> absurd (coerce bot)
{-# INLINE interpret #-}

-- | Like 'interpret', but threads an extra parameter through the interpreters continuation,
-- typically used as state.
--
-- @since 0.1.0.0
stateful ::
  FirstOrder eff =>
  s ->
  (s -> a -> Lang ctx effs b) ->
  (forall x. s -> Lang ctx (eff ': effs) x -> Lang ctx effs (s, x)) ->
  (forall x. s -> eff x -> (s -> x -> Lang ctx effs b) -> Lang ctx effs b) ->
  Lang ctx (eff ': effs) a ->
  Lang ctx effs b
stateful s onRet eta handling = loop s
  where
    loop st = \case
      Pure x -> onRet st x
      Yield (Op op) k -> case decomposeOp op of
        Left other -> Yield (Op other) (k' st)
        Right eff -> handling st eff k'
        where
          k' st' = loop st' . k
      Yield (Scoped scoped l) k -> case decomposeS scoped of
        Left other -> Yield (Scoped other (weave (st, ()) (uncurry eta) l)) (uncurry k')
        Right bot -> absurd (coerce bot)
        where
          k' st' = loop st' . k
