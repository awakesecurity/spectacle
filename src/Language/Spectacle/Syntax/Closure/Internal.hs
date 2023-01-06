{-# LANGUAGE TypeFamilies #-}

-- |
-- Module      :  Language.Spectacle.Syntax.Closure.Internal
-- Copyright   :  (c) Arista Networks, 2022-2023
-- License     :  Apache License 2.0, see LICENSE
--
-- Stability   :  stable
-- Portability :  non-portable (GHC extensions)
--
-- TODO: docs
--
-- @since 1.0.0
module Language.Spectacle.Syntax.Closure.Internal
  ( Closure (Closure),
    Effect (Close),
  )
where

import Data.Void (Void)

import Data.Name (Name)
import Data.Type.Rec (Has)
import Language.Spectacle.Lang (Effect, EffectK, Lang, ScopeK)
import Language.Spectacle.RTS.Registers (StateFun)

-- ---------------------------------------------------------------------------------------------------------------------

newtype Closure :: EffectK where
  Closure :: Void -> Closure a

data instance Effect Closure :: ScopeK where
  Close ::
    (Has s a ctx, m ~ Lang ctx effs) =>
    Name s ->
    StateFun ctx a ->
    Effect Closure m ()
