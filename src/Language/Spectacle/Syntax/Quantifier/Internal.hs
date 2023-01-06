{-# LANGUAGE TypeFamilies #-}

-- |
-- Module      :  Language.Spectacle.Syntax.Quantifier.Internal
-- Copyright   :  (c) Arista Networks, 2022-2023
-- License     :  Apache License 2.0, see LICENSE
--
-- Stability   :  stable
-- Portability :  non-portable (GHC extensions)
--
-- TODO: docs
--
-- @since 1.0.0
module Language.Spectacle.Syntax.Quantifier.Internal
  ( Quantifier (Quantifier),
    Effect (Forall, Exists),
    QuantifierIntro (existsIntro, forallIntro),
  )
where

import Data.Void (Void)

import Language.Spectacle.Lang (Effect, EffectK, Lang, Member, ScopeK, scope)

-- ---------------------------------------------------------------------------------------------------------------------

newtype Quantifier :: EffectK where
  Quantifier :: Void -> Quantifier a

data instance Effect Quantifier :: ScopeK where
  Forall :: [a] -> (a -> m Bool) -> Effect Quantifier m Bool
  Exists :: [a] -> (a -> m Bool) -> Effect Quantifier m Bool

class QuantifierIntro m where
  existsIntro :: [a] -> (a -> m Bool) -> m Bool

  forallIntro :: [a] -> (a -> m Bool) -> m Bool

-- | @since 1.0.0
instance Member Quantifier effs => QuantifierIntro (Lang ctxt effs) where
  forallIntro xs p = scope (Forall xs p)
  {-# INLINE forallIntro #-}

  existsIntro xs p = scope (Exists xs p)
  {-# INLINE existsIntro #-}
