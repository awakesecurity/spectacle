{-# LANGUAGE TypeFamilies #-}

-- |
-- Module      :  Data.Ascript
-- Copyright   :  (c) Arista Networks, 2022-2023
-- License     :  Apache License 2.0, see LICENSE
--
-- Stability   :  stable
-- Portability :  non-portable (GHC extensions)
--
-- Type ascription.
--
-- @since 1.0.0
module Data.Ascript
  ( Ascribe (..),
    type (#),
    type AscriptName,
    type AscriptType,
  )
where

import Data.Kind (Type)
import GHC.TypeLits (Symbol)

-- -------------------------------------------------------------------------------------------------

-- | Type ascriptions.
--
-- @since 0.1.0.0
data Ascribe s a = Ascribe s a

-- | The kind of type ascribed variables in the type row of a 'Data.Type.Rec'.
--
-- @since 0.1.0.0
infix 6 #

type (#) :: Symbol -> k -> Ascribe Symbol k
type s # a = 'Ascribe s a

-- | Project the type of a 'Data.Type.Rec' ascription.
--
-- @since 0.1.0.0
type AscriptType :: Ascribe Symbol Type -> Type
type family AscriptType a where
  AscriptType (_ # a) = a

-- | Project the symbol of a 'Data.Type.Rec' ascription.
--
-- @since 0.1.0.0
type AscriptName :: Ascribe Symbol Type -> Symbol
type family AscriptName a where
  AscriptName (s # _) = s
