{-# LANGUAGE TypeFamilies #-}

-- |
-- Module      :  Language.Spectacle.Syntax.Prime.Internal
-- Copyright   :  (c) Arista Networks, 2022-2023
-- License     :  Apache License 2.0, see LICENSE
--
-- Stability   :  stable
-- Portability :  non-portable (GHC extensions)
--
-- TODO: docs
--
-- @since 1.0.0
module Language.Spectacle.Syntax.Prime.Internal
  ( Prime (Prime),
    Effect (PrimeVar),
  )
where

import Data.Void (Void)

import Data.Type.Rec (Has, Name)
import Language.Spectacle.Lang (Effect, EffectK, Lang)

-- -------------------------------------------------------------------------------------------------

newtype Prime :: EffectK where
  Prime :: Void -> Prime a

data instance Effect Prime m a where
  PrimeVar :: (Has s a ctx, m ~ Lang ctx eff) => Name s -> Effect Prime m a
