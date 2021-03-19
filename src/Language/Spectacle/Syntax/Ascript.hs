-- | Syntactic type ascription primitive.
--
-- @since 0.1.0.0
module Language.Spectacle.Syntax.Ascript
  ( Ascribe (..),
    type (#),
  )
where

import Data.Kind (Type)

import GHC.TypeLits (Symbol)

-- -----------------------------------------------------------------------------

-- | The ascription syntax is used to ascribe types to spectacle names in record
-- rows, for example:
--
-- @
-- type MySpec = #numCookies \# Int
-- @
--
-- @since 0.1.0.0
type Ascribe :: forall i. i -> Type -> Type
data Ascribe nm ty where
  Ascribe :: nm -> ty -> Ascribe nm ty

-- | Infix operator for the kind of ascriptions.
--
-- @since 0.1.0.0
infix 6 #

type (#) :: Symbol -> Type -> Ascribe Symbol Type

type nm # ty = 'Ascribe nm ty
