-- |
--
-- @since 0.1.0.0
module Data.Temporal.Global
  ( -- * Global/Always Modality
    Global (Global),
    getGlobal,
  )
where

import Control.Comonad
import Data.Kind

import Data.Temporal.RSet

-- ---------------------------------------------------------------------------------------------------------------------

newtype Global :: (Type -> Type) -> Type -> Type -> Type where
  Global :: {getGlobal :: w (a -> b)} -> Global w a b
  deriving (Functor)

-- | @since 0.1.0.0
instance (Comonad w, Applicative w) => Applicative (Global w a) where
  pure x = Global (pure (const x))
  {-# INLINE pure #-}

  Global k <*> Global w = Global (extend (\f x -> extract k x (extract f x)) w)
  {-# INLINE (<*>) #-}

-- | @since 0.1.0.0
instance (Monoid a, Comonad w) => Comonad (Global w a) where
  extract (Global f) = extract f mempty
  {-# INLINE extract #-}

  extend f (Global k) = Global (extend (\w xs -> f (Global (fmap (. mappend xs) w))) k)
  {-# INLINE extend #-}
