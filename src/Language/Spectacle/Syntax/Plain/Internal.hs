{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE UndecidableInstances #-}

-- |
-- Module      :  Language.Spectacle.Syntax.Plain.Internal
-- Copyright   :  (c) Arista Networks, 2022-2023
-- License     :  Apache License 2.0, see LICENSE
--
-- Stability   :  stable
-- Portability :  non-portable (GHC extensions)
--
-- TODO: docs
--
-- @since 1.0.0
module Language.Spectacle.Syntax.Plain.Internal
  ( Plain (Plain),
    Effect (PlainVar),
  )
where

import Data.Void (Void)

import Data.Type.Rec (Has, Name)
import Language.Spectacle.Lang (Effect, EffectK, Lang)

-- -------------------------------------------------------------------------------------------------

newtype Plain :: EffectK where
  Plain :: Void -> Plain a

data instance Effect Plain m a where
  PlainVar :: (Has s a ctx, m ~ Lang ctx effs) => Name s -> Effect Plain m a
