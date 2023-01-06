{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE TypeFamilies #-}

-- |
-- Module      :  Language.Spectacle.Syntax.Logic.Internal
-- Copyright   :  (c) Arista Networks, 2022-2023
-- License     :  Apache License 2.0, see LICENSE
--
-- Stability   :  stable
-- Portability :  non-portable (GHC extensions)
--
-- TODO: docs
--
-- @since 1.0.0
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
