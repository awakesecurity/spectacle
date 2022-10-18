{-# LANGUAGE PartialTypeSignatures #-}

-- | Hyperfunction transformer.
--
-- @since 1.0.0
module Control.Hyper
  ( HyperM (HyperM, invokeM),
  )
where

import Data.Kind (Type)

-- ---------------------------------------------------------------------------------------------------------------------

newtype HyperM :: (Type -> Type) -> Type -> Type -> Type where
  HyperM :: {invokeM :: m ((HyperM m a b -> a) -> b)} -> HyperM m a b
