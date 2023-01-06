{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module      :  Data.Name
-- Copyright   :  (c) Arista Networks, 2022-2023
-- License     :  Apache License 2.0, see LICENSE
--
-- Stability   :  stable
-- Portability :  non-portable (GHC extensions)
--
-- @'Name'@s for variables in a Spectacle specification.
--
-- @since 1.0.0
module Data.Name
  ( Name (Name),
    inferName,
  )
where

import Data.Kind (Type)
import Data.Proxy (Proxy (Proxy))
import GHC.OverloadedLabels (IsLabel (fromLabel))
import GHC.TypeLits (KnownSymbol, Symbol, symbolVal)
import Prettyprinter (Pretty, pretty)

-- -------------------------------------------------------------------------------------------------

-- | 'Name' is the type of spectacle names. Using the OverloadedLabels syntax should be used to
-- construct 'Name's.
--
-- @
-- >>> :set -XOverloadedLabels -XPartialTypeSignatures
-- >>> :t #myVar :: 'Name' _
-- >>> #myVar :: Name _ :: Name "myVar"
-- @
--
-- @since 0.1.0.0
data Name :: Symbol -> Type where
  Name :: KnownSymbol s => Proxy s -> Name s

-- | @since 0.1.0.0
instance (KnownSymbol s, l ~ s) => IsLabel l (Name s) where
  -- The @l ~ s@ unification immediately solves @nm@ so that 'IsLabel' doesn't leave it ambiguous
  -- with a wanted constraint.
  fromLabel = Name Proxy

-- | @since 0.1.0.0
instance Show (Name s) where
  show (Name p) = symbolVal p

-- | @since 0.1.0.0
instance Pretty (Name s) where
  pretty name = "#" <> pretty (show name)

-- | @since 0.1.0.0
instance Eq (Name s) where
  -- Nominal equality
  _ == _ = True

-- | Infer a @'Name'@ from a type-level @'Symbol'@
inferName :: KnownSymbol s => Name s
inferName = Name Proxy
{-# INLINE CONLIKE inferName #-}
