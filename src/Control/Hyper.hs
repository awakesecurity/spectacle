
-- |
-- Module      :  Control.Hyper
-- Copyright   :  (c) Arista Networks, 2022-2023
-- License     :  Apache License 2.0, see LICENSE
--
-- Stability   :  stable
-- Portability :  non-portable (GHC extensions)
--
-- Hyperfunction transformer.
--
-- @since 1.0.0
module Control.Hyper
  ( HyperM (HyperM, invokeM),
  )
where

import Data.Kind (Type)

-- ---------------------------------------------------------------------------------------------------------------------

-- | Hyperfunction transformer.
--
-- @since 1.0.0
newtype HyperM :: (Type -> Type) -> Type -> Type -> Type where
  HyperM :: {invokeM :: m ((HyperM m a b -> a) -> b)} -> HyperM m a b
