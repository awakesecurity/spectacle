{-# LANGUAGE RecordWildCards #-}

-- | 'Pos' is pair of integers representing a position in grid. The implementation for 'compare' on 'Pos' is biased to
-- the row-value of the position, i.e.
--
-- @
-- ∀ (x y u v :: Int) -> x ≢ u -> Pos x y `compare` Pos u v ≡ compare x u
-- @
--
-- This definition of 'compare' enables collections of 'Pos' to preserve most of the structural information in a rose
-- tree when kept in ordered-containers who manage their internal structure with 'compare' such as 'Set'. To motivate
-- why this is useful, it is worth noting that the model checker works exclusively with nested structures like rose
-- trees. Rendering output from trees is difficult since it is not always clear how to traverse a tree to extract a
-- flattened presentation of information in a tree that can be easily rendered as output.
--
-- Traversing a ('Set' 'Pos') is trivial, though, since 'Set' is (for all intent and purposes) a flat collection of
-- elements, and more specifically one which manages its structure with 'compare' internally. As a result of the way in
-- which 'compare' is defined, column and row values of 'Pos' can simultaneously be viewed as file locations and
-- node positions in a tree. Sets of Pos are flat organizations of information that can be rendered to output
-- straightforward way while still being able to store/recover the shape information of the original tree we're
-- interested in logging:
--
-- @
--   1  2  3  4  5  6  7  8  9
-- ┌╌🮻╌╌🮻╌╌🮻╌ ○ ╌🮻╌╌🮻╌╌🮻╌╌🮻╌╌🮻╌┐
-- ┆ ┆  ┆   ╱ ┃ ╲   ┆  ┆  ┆  ┆ ┆
-- ├╌🮻╌╌🮻╌ ○  ○  ○   ╌╌🮻╌╌🮻╌╌🮻╌┤ 1
-- ┆ ┆  ┆  ┃  ┃  ┃ ╲   ┆  ┆  ┆ ┆
-- ├╌🮻╌╌🮻╌ ○  ○  ○  ○ ╌🮻╌╌🮻╌╌🮻╌┤ 2
-- ┆ ┆  ┆   ╲ ┃  ┃ ╱   ┆  ┆  ┆ ┆
-- ├╌🮻╌╌🮻╌    ○  ○   ╌╌🮻╌╌🮻╌╌🮻╌┤ 3
-- ┆ ┆  ┆  ┆  ┃  ┃  ┆  ┆  ┆  ┆ ┆
-- ├╌🮻╌╌🮻╌╌🮻╌ ○  ○ ╌🮻╌╌🮻╌╌🮻╌╌🮻╌┤ 4
-- ┆ ┆  ┆  ┆  ┃ ╱   ┆  ┆  ┆  ┆ ┆
-- ├╌🮻╌╌🮻╌╌🮻╌ ○ ╌🮻╌╌🮻╌╌🮻╌╌🮻╌╌🮻╌┤ 5
-- ┆ ┆  ┆  ┆  ┃  ┆  ┆  ┆  ┆  ┆ ┆
-- └╌🮻╌╌🮻╌╌🮻╌ ○ ╌🮻╌╌🮻╌╌🮻╌╌🮻╌╌🮻╌┘
-- @
--
-- This example would be the set with elements ordered according to their depth, and then to their span:
--
-- @
-- fromList [(0,4), (1,3), (1,4), (1,5), (2,3), (2,4), (2,5), ..., (5,4), (6,4)]
-- @
--
-- @since 0.1.0.0
module Language.Spectacle.Interaction.Pos
  ( -- * Locations
    Loc (Loc),
    locPos,
    locExt,

    -- * Positions
    Pos (MkPos),
    pattern Pos,
    getPos,
    posRow,
    posCol,

    -- ** Lenses
    prow,
    pcol,
  )
where

import Data.Function (on)
import Data.Semigroup (Sum (Sum))
import Lens.Micro (Lens', lens, set, _1, _2)
import Lens.Micro.Extras (view)

-- ---------------------------------------------------------------------------------------------------------------------

-- | A 'Loc' is a position annotated with an length/span.
--
-- @since 0.1.0.0
data Loc = Loc
  { locPos :: {-# UNPACK #-} !Pos
  , locExt :: {-# UNPACK #-} !Int
  }
  deriving (Eq)

-- | @since 0.1.0.0
instance Ord Loc where
  compare = compare `on` locPos

-- | Buffer row/column positions.
--
-- @since 0.1.0.0
newtype Pos = MkPos {getPos :: (Int, Int)}
  deriving (Eq, Semigroup, Monoid) via (Sum Int, Sum Int)

pattern Pos :: Int -> Int -> Pos
pattern Pos {posRow, posCol} = MkPos (posRow, posCol)

{-# COMPLETE Pos #-}

-- | @since 0.1.0.0
instance Ord Pos where
  -- The derived implementation of 'Ord' is equivalent to what is given here, but its defined explicitly anyway
  -- to emphasize the row value superseding the order of the column value in this trichotomy.
  Pos x y `compare` Pos u v = case compare x u of
    EQ -> compare y v
    ordering -> ordering

-- | @since 0.1.0.0
instance Show Pos where
  show (Pos r c) = "Pos(" ++ show r ++ ":" ++ show c ++ ")"

prow :: Lens' Pos Int
prow = lens (view _1 . getPos) \ ~(MkPos p) x -> MkPos (set _1 x p)
{-# INLINE prow #-}

pcol :: Lens' Pos Int
pcol = lens (view _2 . getPos) \ ~(MkPos p) x -> MkPos (set _2 x p)
{-# INLINE pcol #-}
