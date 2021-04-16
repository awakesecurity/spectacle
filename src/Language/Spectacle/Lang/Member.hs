{-# LANGUAGE TypeFamilies #-}

-- | Effect membership.
--
-- @since 0.1.0.0
module Language.Spectacle.Lang.Member
  ( Members,
    Member (inject, project, injectS, projectS),
  )
where

import Data.Kind (Constraint)

import Language.Spectacle.Lang.Op (Op (OHere, OThere))
import Language.Spectacle.Lang.Scoped (Effect, EffectK, Scoped (SHere, SThere))

-- -------------------------------------------------------------------------------------------------

-- | N-ary, infix operator for 'Member'.
--
-- * @Members '[A, B, C] effs = (Member A effs, Member B effs, Member C effs)@
-- * @Members A effs = Members '[A] effs = Member A effs@
--
-- @since 0.1.0.0
type Members :: forall k. k -> [EffectK] -> Constraint
type family Members eff effs where
-- Type ascription is used here rather than a type application to the LHS of the equations since
-- fourmolu does not know how to parse type applications at the type level yet.
--
-- https://github.com/tweag/ormolu/issues/698
  Members (eff :: EffectK) effs = Member eff effs
  Members (eff ': effs' :: [EffectK]) effs = (Member eff effs, Members effs' effs)
  Members ('[] :: [EffectK]) effs = ()

-- | An effect @eff@ is a member of the effect signature @effs@ if @eff@ occurs in @effs@.
--
-- @since 0.1.0.0
type Member :: EffectK -> [EffectK] -> Constraint
class Member eff effs where
  -- | Inject a first order effect @eff a@ into a sum of first-order effects 'Op'. Higher-order
  -- operations must be injected with 'injectS'.
  --
  -- @since 0.1.0.0
  inject :: eff a -> Op effs a

  -- | Projects the effect @eff@ from the given 'Op' if it is the inhabitant, otherwise 'Nothing'.
  --
  -- @since 0.1.0.0
  project :: Op effs a -> Maybe (eff a)

  -- | Like 'inject', but specifically for scoped operations.
  --
  -- @since 0.1.0.0
  injectS :: Effect eff m a -> Scoped effs m a

  -- | Like 'project', but specifically for scoped operations.
  --
  -- @since 0.1.0.0
  projectS :: Scoped effs m a -> Maybe (Effect eff m a)

-- | @since 0.1.0.0
instance {-# OVERLAPS #-} Member eff (eff ': effs) where
  inject = OHere
  {-# INLINE CONLIKE inject #-}

  project (OHere op) = Just op
  -- We only have to scrutinize this case because there is technically nothing preventing an effect
  -- signature from having two of the same effect in the list; however, the only way to make that
  -- happen is to instantiate Lang with a monomorphic list containing duplicates. Nubbing effect
  -- signatures isn't worth it in any case since running handlers is idempotent so this case can
  -- just be thrown out.
  project (OThere _) = Nothing
  {-# INLINE CONLIKE project #-}

  injectS = SHere
  {-# INLINE CONLIKE injectS #-}

  projectS (SHere s) = Just s
  projectS (SThere _) = Nothing
  {-# INLINE CONLIKE projectS #-}

-- | @since 0.1.0.0
instance Member eff effs => Member eff (eff' ': effs) where
  inject = OThere . inject
  {-# INLINE CONLIKE inject #-}

  project _ = Nothing
  {-# INLINE CONLIKE project #-}

  injectS = SThere . injectS
  {-# INLINE CONLIKE injectS #-}

  projectS _ = Nothing
  {-# INLINE CONLIKE projectS #-}
