{-# LANGUAGE TypeFamilies #-}

-- |
-- Module      :  Language.Spectacle.Syntax.Env.Internal
-- Copyright   :  (c) Arista Networks, 2022-2023
-- License     :  Apache License 2.0, see LICENSE
--
-- Stability   :  stable
-- Portability :  non-portable (GHC extensions)
--
-- TODO: docs
--
-- @since 1.0.0
module Language.Spectacle.Syntax.Env.Internal
  ( Env (Env),
    Effect (Get, Put),
  )
where

import Data.Void (Void)

import Language.Spectacle.Lang (Effect, EffectK)
import Language.Spectacle.RTS.Registers (RuntimeState)

-- ---------------------------------------------------------------------------------------------------------------------

newtype Env :: EffectK where
  Env :: Void -> Env a

data instance Effect Env m a where
  Get :: m ~ f ctx effs => Effect Env m (RuntimeState ctx)
  Put :: m ~ f ctx effs => RuntimeState ctx -> Effect Env m ()
