{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE UndecidableInstances #-}

-- |
--
-- @since 0.1.0.0
module Language.Spectacle.Interaction.Point
  ( -- * Points
    Point (Point),
    pointLabel,
    pointFields,
    pointPos,
    pointPar,
    pointLen,

    -- ** Construction
    fromWorld,

    -- ** Lenses
    label,
    parent,
    fields,
    column,
    row,
    extent,
  )
where

import Data.Function (on)
import Lens.Micro (Lens', SimpleGetter, lens, to, (^.))
import Prettyprinter (Doc, pretty, viaShow, (<+>))
import Prettyprinter.Render.Terminal (AnsiStyle)

import Data.Type.Rec (HasDict, Rec, evident, pattern ConE, pattern NilE)
import Data.World (World (World))
import Data.Fingerprint (Fingerprint)
import Language.Spectacle.Interaction.Pos (Pos, pcol, prow, pattern Pos)

-- ---------------------------------------------------------------------------------------------------------------------

data Point = Point
  { pointLabel :: {-# UNPACK #-} !Fingerprint
  , pointFields :: [Doc AnsiStyle]
  , pointPar :: !(Maybe Fingerprint)
  , pointLen :: {-# UNPACK #-} !Int
  , pointPos :: {-# UNPACK #-} !Pos
  }
  deriving Show

fromWorld :: HasDict Show ctx => World ctx -> Point
fromWorld (World hash fs0) = Point hash (docFields fs0) Nothing 0 (Pos 0 0)
  where
    docFields :: HasDict Show ctx => Rec ctx -> [Doc AnsiStyle]
    docFields rs =
      case evident @Show rs of
        ConE n x xs -> pretty n <+> "=" <+> viaShow x : docFields xs
        NilE -> []

-- | @since 0.1.0.0
instance Eq Point where
  pt0 == pt1 =
    let lblEq = pt0 ^. label == pt1 ^. label
        posEq = pt0 ^. position == pt1 ^. position
     in lblEq && posEq
  {-# INLINE (==) #-}

-- | @since 0.1.0.0
instance Ord Point where
  compare x y = case (compare `on` pointPos) x y of
    EQ -> (compare `on` pointLabel) x y
    ordering -> ordering
  {-# INLINE compare #-}

-- ---------------------------------------------------------------------------------------------------------------------

label :: SimpleGetter Point Fingerprint
label = to pointLabel
{-# INLINE label #-}

fields :: SimpleGetter Point [Doc AnsiStyle]
fields = to pointFields
{-# INLINE fields #-}

parent :: Lens' Point (Maybe Fingerprint)
parent = lens pointPar \pt par -> pt {pointPar = par}
{-# INLINE parent #-}

position :: Lens' Point Pos
position = lens pointPos \pt p -> pt {pointPos = p}
{-# INLINE position #-}

column :: Lens' Point Int
column = position . pcol
{-# INLINE column #-}

row :: Lens' Point Int
row = position . prow
{-# INLINE row #-}

extent :: Lens' Point Int
extent = lens pointLen \pt i -> pt {pointLen = i}
{-# INLINE extent #-}
