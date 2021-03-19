module Data.Var
  ( Var (..),
  )
where

import Data.Hashable (Hashable, hash)
import Data.Kind (Type)
import GHC.Generics (Generic)
import GHC.TypeLits (Symbol)

-- -----------------------------------------------------------------------------

-- | A 'Var' wraps a @t@ with a label @s@ for the variable's name.
--
-- @since 0.1.0.0
type role Var representational representational

type Var :: Symbol -> Type -> Type
data Var s t = Var t
  deriving (Eq, Ord, Generic, Show)

-- | @since 0.1.0.0
instance Hashable t => Hashable (Var s t) where
  hash (Var x) = hash x
