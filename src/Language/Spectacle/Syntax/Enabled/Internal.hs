{-# LANGUAGE TypeFamilies #-}

-- |
-- Module      :  Language.Spectacle.Syntax.Enabled.Internal
-- Copyright   :  (c) Arista Networks, 2022-2023
-- License     :  Apache License 2.0, see LICENSE
--
-- Stability   :  stable
-- Portability :  non-portable (GHC extensions)
--
-- TODO: docs
--
-- @since 1.0.0
module Language.Spectacle.Syntax.Enabled.Internal
  ( Enabled (Enabled),
    Effect (EnabledS),
  )
where

import Data.Void (Void)

import Language.Spectacle.Lang (Effect, EffectK, ScopeK)

-- ---------------------------------------------------------------------------------------------------------------------

data Enabled :: EffectK where
  Enabled :: Enabled Bool

newtype instance Effect Enabled :: ScopeK where
  EnabledS :: Void -> Effect Enabled m a
