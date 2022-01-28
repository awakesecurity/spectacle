{-# LANGUAGE AllowAmbiguousTypes #-}

-- |
--
-- @since 0.1.0.0
module Language.Spectacle.Specification.Prop
  ( -- * Temporal Formula
    TemporalType (PropG, PropF, PropGF, PropFG),

    -- ** Projection
    toFormula,
    toModality,

    -- * Temporal Operators
    Modality (Always, Infinitely, Eventually, Stays),
  )
where

import Data.Kind (Type)
import GHC.TypeLits (Symbol)

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
