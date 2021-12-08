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
  ( -- * Buffer Positions
    Pos (Pos),
    posRow,
    posCol,

    -- ** Lenses
    prow,
    pcol,

    -- * Buffer Intervals/Spans
    Interval (Interval),
    intervalMin,
    intervalMax,

    -- ** Construction
    fromDiff,
    fromSpan,

    -- ** Lenses
    ivmax,
    ivmin,
  )
where

import Lens.Micro (Lens', SimpleGetter, to, lens)

-- ---------------------------------------------------------------------------------------------------------------------

-- | Buffer row/column positions.
--
-- @since 0.1.0.0
data Pos = Pos
  { posRow :: {-# UNPACK #-} !Int
  , posCol :: {-# UNPACK #-} !Int
  }
  deriving (Eq)

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
prow = lens posRow \Pos {..} r ->
  Pos {posRow = r, ..}
{-# INLINE prow #-}

pcol :: Lens' Pos Int
pcol = lens posCol \Pos {..} c ->
  Pos {posCol = c, ..}
{-# INLINE pcol #-}

-- ---------------------------------------------------------------------------------------------------------------------

data Interval = Interval
  { intervalMin :: {-# UNPACK #-} !Int
  , intervalMax :: {-# UNPACK #-} !Int
  }
  deriving Eq

-- | Construct an 'Interval' from a minimal point and a distance from the maximal value. No guards are in place to
-- ensure the values provided make sense.
--
-- @since 0.1.0.0
fromDiff :: Int -> Int -> Interval
fromDiff x dist = Interval x (x + dist)

-- | Constructs an 'Interval' from a minimal point and the length of a 'Foldable' container.
--
-- @since 0.1.0.0
fromSpan :: Foldable t => Int -> t a -> Interval
fromSpan x = fromDiff x . length

-- | @since 0.1.0.0
instance Show Interval where
  show (Interval x y) = "Interval(" ++ show x ++ "," ++ show y ++ ")"

ivmax :: SimpleGetter Interval Int
ivmax = to intervalMax
{-# INLINE ivmax #-}

ivmin :: SimpleGetter Interval Int
ivmin = to intervalMin
{-# INLINE ivmin #-}
