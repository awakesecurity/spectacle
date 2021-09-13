{-# LANGUAGE TypeFamilies #-}

module Language.Spectacle.Syntax.Quantifier.Internal (
    Quantifier (Quantifier),
    Effect (Forall, Exists),
    QuantifierIntro (existsIntro, forallIntro),
) where

import Data.Void (Void)

import Language.Spectacle.Lang (Effect, EffectK, ScopeK, Member, Lang, scope)

-- ---------------------------------------------------------------------------------------------------------------------

newtype Quantifier :: EffectK where
    Quantifier :: Void -> Quantifier a

data instance Effect Quantifier :: ScopeK where
    Forall :: [a] -> (a -> m Bool) -> Effect Quantifier m Bool
    Exists :: [a] -> (a -> m Bool) -> Effect Quantifier m Bool

class QuantifierIntro m where
  existsIntro :: [a] -> (a -> m Bool) -> m Bool

  forallIntro :: [a] -> (a -> m Bool) -> m Bool

-- | @since 0.1.0.0
instance Member Quantifier effs => QuantifierIntro (Lang ctxt effs) where
  forallIntro xs p = scope (Forall xs p)
  {-# INLINE forallIntro #-}

  existsIntro xs p = scope (Exists xs p)
  {-# INLINE existsIntro #-}
