{-# LANGUAGE TypeFamilies #-}

module Language.Spectacle.Syntax.Enabled.Internal
  ( Enabled (Enabled),
    Effect (EnabledS),
  )
where

import Data.Void (Void)

import Language.Spectacle.Lang (Effect, EffectK, Lang, ScopeK)

-- ---------------------------------------------------------------------------------------------------------------------

data Enabled :: EffectK where
  Enabled :: Enabled Bool

newtype instance Effect Enabled :: ScopeK where
  EnabledS :: Void -> Effect Enabled m a
