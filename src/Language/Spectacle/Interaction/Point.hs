{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE UndecidableInstances #-}

-- |
--
-- @since 0.1.0.0
module Language.Spectacle.Interaction.Point
  ( -- * Points
    Point (Point),
    getPoint,
    pointPar,
    pointPos,

    -- ** Construction
    pattern RootPoint,

    -- ** Lenses
    ptlabel,
    ptpos,
    ptinterval,
  )
where

import Data.Function (on)
import Lens.Micro (Lens', SimpleGetter, (^.), lens, to)

import Data.Type.Rec (Rec)
import Data.World (World, worldFingerprint)
import Language.Spectacle.Checker.Fingerprint (Fingerprint)
import Language.Spectacle.Interaction.Pos (Interval, Pos)

-- ---------------------------------------------------------------------------------------------------------------------

data Point ctx = Point
  { getPoint :: World ctx
  , pointPar :: Maybe (Point ctx)
  , pointItv :: {-# UNPACK #-} !Interval
  , pointPos :: {-# UNPACK #-} !Pos
  }

-- | @since 0.1.0.0
instance Show (Rec ctx) => Show (Point ctx) where
  show (Point w Nothing i p) = "RootPoint(" ++ show p ++ "@" ++ show i ++ "): " ++ show w
  show (Point w (Just par) i p) =
    "Point("
      ++ show p
      ++ "@"
      ++ show i
      ++ ", "
      ++ show (par ^. ptlabel)
      ++ "): "
      ++ show w

-- | @since 0.1.0.0
instance Eq (Point ctx) where
  (==) = (==) `on` pointPos
  {-# INLINE (==) #-}

-- | @since 0.1.0.0
instance Ord (Point ctx) where
  compare = compare `on` pointPos
  {-# INLINE compare #-}

ptlabel :: SimpleGetter (Point ctx) Fingerprint
ptlabel = to getPoint . worldFingerprint
{-# INLINE ptlabel #-}

ptpos :: Lens' (Point ctx) Pos
ptpos = lens pointPos \pt p ->
  pt {pointPos = p}
{-# INLINE ptpos #-}

ptinterval :: Lens' (Point ctx) Interval
ptinterval = lens pointItv \pt i ->
  pt {pointItv = i}
{-# INLINE ptinterval #-}

-- ---------------------------------------------------------------------------------------------------------------------

-- | Pattern synonym for constructing a root 'Point' (omits a parent 'Point').
--
-- @since 0.1.0.0
pattern RootPoint :: World ctx -> Interval -> Pos -> Point ctx
pattern RootPoint w i p = Point w Nothing i p
