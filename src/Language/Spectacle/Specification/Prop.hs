{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module      :  Language.Spectacle.Specification.Prop
-- Copyright   :  (c) Arista Networks, 2022-2023
-- License     :  Apache License 2.0, see LICENSE
--
-- Stability   :  stable
-- Portability :  non-portable (GHC extensions)
--
-- TODO: docs
--
-- @since 1.0.0
module Language.Spectacle.Specification.Prop
  ( -- * Temporal Formula
    TemporalType (PropG, PropF, PropGF, PropFG),

    -- ** Projection
    toFormula,
    toModality,

    -- * Temporal Operators
    Modality (Always, Infinitely, Eventually, Stays),

    -- ** Pretty Printing
    ppModality,
  )
where

import Data.Kind (Type)
import GHC.TypeLits (Symbol)
import Prettyprinter (Doc)
import Prettyprinter.Render.Terminal (AnsiStyle)

import Data.Ascript (Ascribe)
import Language.Spectacle.AST.Temporal (Temporal)

-- ---------------------------------------------------------------------------------------------------------------------

data TemporalType :: [Ascribe Symbol Type] -> Modality -> Type where
  PropG :: Temporal ctx Bool -> TemporalType ctx 'Always
  PropF :: Temporal ctx Bool -> TemporalType ctx 'Eventually
  PropGF :: Temporal ctx Bool -> TemporalType ctx 'Infinitely
  PropFG :: Temporal ctx Bool -> TemporalType ctx 'Stays

toFormula :: TemporalType ctx op -> Temporal ctx Bool
toFormula = \case
  PropG form -> form
  PropF form -> form
  PropGF form -> form
  PropFG form -> form

toModality :: TemporalType ctx op -> Modality
toModality = \case
  PropG {} -> Always
  PropF {} -> Eventually
  PropGF {} -> Infinitely
  PropFG {} -> Stays

data Modality
  = Always
  | Eventually
  | Infinitely
  | Stays
  deriving stock (Eq, Enum, Ord, Show)

ppModality :: Modality -> Doc AnsiStyle
ppModality = \case
  Always -> "always"
  Eventually -> "eventually"
  Infinitely -> "infinitely"
  Stays -> "stays-as"
