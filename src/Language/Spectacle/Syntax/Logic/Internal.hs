{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TypeFamilies #-}

module Language.Spectacle.Syntax.Logic.Internal
  ( Logic (Logic),
    Effect (Complement, Conjunct, Disjunct),
  )
where

import Data.Void (Void)

import Language.Spectacle.Lang (Effect, EffectK, ScopeK)

-- ---------------------------------------------------------------------------------------------------------------------

newtype Logic :: EffectK where
  Logic :: Void -> Logic a

data instance Effect Logic :: ScopeK where
  Complement :: m Bool -> Effect Logic m Bool
  Conjunct :: m Bool -> m Bool -> Effect Logic m Bool
  Disjunct :: m Bool -> m Bool -> Effect Logic m Bool
