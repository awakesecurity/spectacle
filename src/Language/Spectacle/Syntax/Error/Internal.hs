{-# LANGUAGE TypeFamilies #-}

-- |
-- Module      :  Language.Spectacle.Syntax.Error.Internal
-- Copyright   :  (c) Arista Networks, 2022-2023
-- License     :  Apache License 2.0, see LICENSE
--
-- Stability   :  stable
-- Portability :  non-portable (GHC extensions)
--
-- TODO: docs
--
-- @since 1.0.0
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
