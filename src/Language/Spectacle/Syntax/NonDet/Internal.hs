{-# LANGUAGE TypeFamilies #-}

-- |
-- Module      :  Language.Spectacle.Syntax.NonDet.Internal
-- Copyright   :  (c) Arista Networks, 2022-2023
-- License     :  Apache License 2.0, see LICENSE
--
-- Stability   :  stable
-- Portability :  non-portable (GHC extensions)
--
-- TODO: docs
--
-- @since 1.0.0
module Language.Spectacle.Syntax.NonDet.Internal
  ( NonDet (Empty, Choose),
    Effect (MSplit),
  )
where

import Language.Spectacle.Lang.Member (Member)
import Language.Spectacle.Lang.Scoped (Effect)

-- ---------------------------------------------------------------------------------------------------------------------

data NonDet a where
  Empty :: NonDet a
  Choose :: NonDet Bool

data instance Effect NonDet m a where
  MSplit :: (m ~ f ctx effs, Member NonDet effs) => m a -> Effect NonDet m (Maybe (m a, m a))
