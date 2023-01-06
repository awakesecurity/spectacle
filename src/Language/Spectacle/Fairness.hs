{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilyDependencies #-}

-- |
-- Module      :  Language.Spectacle.Fairness
-- Copyright   :  (c) Arista Networks, 2022-2023
-- License     :  Apache License 2.0, see LICENSE
--
-- Stability   :  stable
-- Portability :  non-portable (GHC extensions)
--
-- TODO: docs
--
-- @since 1.0.0
module Language.Spectacle.Fairness
  ( -- * Fairness
    Fairness (Unfair, WeakFair, StrongFair),

    -- ** Reification
    reifyFairness,
  )
where

import Data.Proxy (Proxy (Proxy))
import Type.Reflection (Typeable, someTypeRep)

-- ---------------------------------------------------------------------------------------------------------------------

-- | Enumerated fairness constraints.
--
-- @since 1.0.0
data Fairness
  = Unfair
  | WeakFair
  | StrongFair
  deriving (Enum, Eq, Ord, Show, Typeable)

-- | Reifies a promoted 'Fairness' constructor.
--
-- @since 1.0.0
reifyFairness :: forall (x :: Fairness). Typeable x => Fairness
reifyFairness
  | actualTyCon == strongTyCon = StrongFair
  | actualTyCon == weakTyCon = WeakFair
  | otherwise = Unfair
  where
    actualTyCon = someTypeRep (Proxy @x)
    strongTyCon = someTypeRep (Proxy @'StrongFair)
    weakTyCon = someTypeRep (Proxy @'WeakFair)
