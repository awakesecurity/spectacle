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

    -- ** Comparison
    sameLabel,

    -- ** Lenses
    ptparent,
    ptlabel,
    ptpos,
    ptspan,
  )
where

import Data.Function (on)
import Lens.Micro (Lens', SimpleGetter, lens, to)
import Lens.Micro.Extras (view)

import Data.Type.Rec (HasDict)
import Data.World (World, worldFingerprint)
import Language.Spectacle.Checker.Fingerprint (Fingerprint)
import Language.Spectacle.Interaction.Pos (Pos)

-- ---------------------------------------------------------------------------------------------------------------------

data Point ctx = Point
  { getPoint :: World ctx
  , pointPar :: Maybe (Point ctx)
  , pointLen :: {-# UNPACK #-} !Int
  , pointPos :: {-# UNPACK #-} !Pos
  }

-- | @since 0.1.0.0
deriving instance HasDict Show ctx => Show (Point ctx)

-- | @since 0.1.0.0
instance Eq (Point ctx) where
  (==) = (==) `on` pointPos
  {-# INLINE (==) #-}

-- | @since 0.1.0.0
instance Ord (Point ctx) where
  compare x y = case (compare `on` pointPos) x y of
    EQ -> (compare `on` getPoint) x y
    ordering -> ordering
  {-# INLINE compare #-}

sameLabel :: Point ctx -> Point ctx -> Bool
sameLabel = (==) `on` view ptlabel

ptparent :: SimpleGetter (Point ctx) (Maybe (Point ctx))
ptparent = to pointPar
{-# INLINE ptparent #-}

ptlabel :: SimpleGetter (Point ctx) Fingerprint
ptlabel = to getPoint . worldFingerprint
{-# INLINE ptlabel #-}

ptpos :: Lens' (Point ctx) Pos
ptpos = lens pointPos \pt p ->
  pt {pointPos = p}
{-# INLINE ptpos #-}

ptspan :: Lens' (Point ctx) Int
ptspan = lens pointLen \pt i ->
  pt {pointLen = i}
{-# INLINE ptspan #-}

-- ---------------------------------------------------------------------------------------------------------------------

-- | Pattern synonym for constructing a root 'Point' (omits a parent 'Point').
--
-- @since 0.1.0.0
pattern RootPoint :: World ctx -> Int -> Pos -> Point ctx
pattern RootPoint w i p = Point w Nothing i p
