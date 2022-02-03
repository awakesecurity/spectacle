{-# LANGUAGE PartialTypeSignatures #-}
-- Needed for MonadZip (LogicT m)
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}

-- | Hyperfunction transformer.
--
-- @since 1.0.0
module Control.Hyper
  ( HyperM (HyperM, invokeM),
  )
where

import Control.Monad (join)
import Control.Monad.Logic (LogicT (LogicT))
import Control.Monad.Zip (MonadZip, mzipWith)
import Data.Kind (Type)

-- ---------------------------------------------------------------------------------------------------------------------

newtype HyperM :: (Type -> Type) -> Type -> Type -> Type where
  HyperM :: {invokeM :: m ((HyperM m a b -> a) -> b)} -> HyperM m a b

-- | @since 1.0.0
instance Monad m => MonadZip (LogicT m) where
  mzipWith op (LogicT f) (LogicT g) = LogicT \cons nil ->
    let fs x xs = pure (\k -> k (HyperM xs) x)

        gs y ys = pure (\k x -> cons (op x y) (join (invokeM k <*> ys)))
     in join (f fs (pure (const nil)) <*> g gs (pure \_ _ -> nil))
  {-# INLINE mzipWith #-}
