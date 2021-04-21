{-# LANGUAGE TypeFamilies #-}

module Language.Spectacle.Syntax.Error.Internal
  ( Error (ThrowE),
    Effect (CatchE),
  )
where

import Language.Spectacle.Lang (Effect, EffectK, ScopeK)

-- -------------------------------------------------------------------------------------------------

newtype Error e :: EffectK where
  ThrowE :: e -> Error e a

data instance Effect (Error e) :: ScopeK where
  CatchE :: m a -> (e -> m a) -> Effect (Error e) m a
