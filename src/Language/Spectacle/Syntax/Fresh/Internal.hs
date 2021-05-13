{-# LANGUAGE TypeFamilies #-}

module Language.Spectacle.Syntax.Fresh.Internal
  ( Fresh (Fresh),
    Effect (FreshH),
  )
where

import Data.Void (Void)

import Language.Spectacle.Lang (Effect, EffectK, ScopeK)

-- ---------------------------------------------------------------------------------------------------------------------

data Fresh :: EffectK where
  Fresh :: Fresh Int

newtype instance Effect Fresh :: ScopeK where
  FreshH :: Void -> Effect Fresh m a
