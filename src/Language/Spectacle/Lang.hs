{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeFamilies #-}

-- | The 'Lang' monad and functions for defining Spectacles syntax as effects.
--
-- @since 1.0.0
module Language.Spectacle.Lang
  ( -- * Lang
    Lang (Pure, Op, Scoped),
    runLang,
    send,
    scope,
    weaken,

    -- * Effects
    type EffectK,
    type ScopeK,
    FirstOrder,
    Effect,

    -- ** Membership
    type Members,
    Member (inject, project, injectS, projectS),

    -- ** Unions
    Op (OHere, OThere),
    Scoped (SHere, SThere),
    decomposeOp,
    extractOp,
    decomposeS,
    extractS,
  )
where

import Data.Functor.Loom (hoist, (~>~))
import Language.Spectacle.Lang.Internal (Lang (Op, Pure, Scoped), scope, send)
import Language.Spectacle.Lang.Member (Member (inject, injectS, project, projectS), type Members)
import Language.Spectacle.Lang.Op (Op (OHere, OThere), decomposeOp, extractOp)
import Language.Spectacle.Lang.Scoped
  ( Effect,
    EffectK,
    FirstOrder,
    ScopeK,
    Scoped (SHere, SThere),
    decomposeS,
    extractS,
  )

-- ---------------------------------------------------------------------------------------------------------------------

-- | Used to unwrap the pure value in 'Lang' after all of its effects have been discharged.
--
-- @since 1.0.0
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

-- | Appends an effect label @eff@ to the head of a 'Lang's effect signature by the weakening rule
-- for sum types.
--
-- @since 1.0.0
weaken :: forall eff effs ctx a. Lang ctx effs a -> Lang ctx (eff ': effs) a
weaken = \case
  Pure x -> pure x
  Op op k -> Op (OThere op) (weaken . k)
  Scoped scoped loom -> Scoped (SThere scoped) (loom ~>~ hoist weaken)
