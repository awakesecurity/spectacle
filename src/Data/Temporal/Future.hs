{-# LANGUAGE GeneralizedNewtypeDeriving #-}

-- |
--
-- @since 0.1.0.0
module Data.Temporal.Future
  ( -- * Future/Eventually Modality
    Future (Future),
  )
where

import Control.Arrow (Arrow, Kleisli)
import Control.Monad
import Data.Distributive
import Data.Functor.Rep
import Data.Kind
import GHC.Generics

import Data.Temporal.RSet

-- ---------------------------------------------------------------------------------------------------------------------

newtype Future :: (Type -> Type) -> Type -> Type -> Type where
  Future :: {getFuture :: a -> m b} -> Future m a b
  deriving (Functor, Generic, Generic1)

-- | @since 0.1.0.0
instance Applicative m => Applicative (Future m a) where
  pure x = Future (const (pure x))
  {-# INLINE pure #-}

  Future k <*> Future f = Future \x -> k x <*> f x
  {-# INLINE (<*>) #-}

-- | @since 0.1.0.0
instance Monad m => Monad (Future m a) where
  Future f >>= k = Future \x -> f x >>= \x' -> getFuture (k x') x
  {-# INLINE (>>=) #-}

-- | @since 0.1.0.0
instance (Functor m, Distributive m) => Distributive (Future m a) where
  distribute fg = Future \x -> distribute (fmap (($ x) . getFuture) fg)
  {-# INLINE distribute #-}

-- | @since 0.1.0.0
instance (Representable m, Functor m) => Representable (Future m a)
