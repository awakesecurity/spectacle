-- | Syntactic name primitive.
--
-- @since 0.1.0.0
module Language.Spectacle.Syntax.Name
  ( Name (..),
    reifyName,
    isPrimedName,
    inferName,
  )
where

import Data.Kind (Type)
import Data.Proxy (Proxy (Proxy))
import GHC.OverloadedLabels (IsLabel (fromLabel))
import GHC.TypeLits (KnownSymbol, Symbol, symbolVal)

-- -----------------------------------------------------------------------------

-- | 'Name' is the type of spectacle names. Using the OverloadedLabels syntax is
-- preferred for constructing 'Name':
--
-- @
-- >>> :set -XOverloadedLabels -XPartialTypeSignatures
-- >>> :t #myVar :: 'Name' _
-- >>> #myVar :: Name _ :: Name "myVar"
-- @
--
-- @since 0.1.0.0
type Name :: Symbol -> Type
data Name nm where
  Name :: KnownSymbol nm => Proxy nm -> Name nm

-- | @since 0.1.0.0
instance Eq (Name nm) where
  _ == _ = True -- Nominal equality

-- | @since 0.1.0.0
instance Show (Name nm) where
  show (Name p) = "#" ++ symbolVal p

-- | @since 0.1.0.0
instance (l ~ nm, KnownSymbol nm) => IsLabel l (Name nm) where
  -- The @l ~ nm@ unification immediately solves @nm@ so that 'IsLabel' doesn't
  -- leave it ambiguous with a wanted constraint.
  fromLabel = Name Proxy

-- | Reify the typelevel 'Symbol' representing a name as a string.
--
-- @since 0.1.0.0
reifyName :: Name nm -> String
reifyName (Name p) = symbolVal p
{-# INLINE reifyName #-}

-- | Predicate for whether a given name ends in a single quote.
--
-- @since 0.1.0.0
isPrimedName :: Name nm -> Bool
isPrimedName nm = last (reifyName nm) == '\''
{-# INLINE isPrimedName #-}

-- | Construct a 'Name' whose symbol is inferred.
--
-- @since 0.1.0.0
inferName :: KnownSymbol nm => Name nm
inferName = Name Proxy
{-# INLINE CONLIKE inferName #-}
