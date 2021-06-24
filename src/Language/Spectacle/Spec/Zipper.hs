module Language.Spectacle.Spec.Zipper
  ( -- * Disjunction Zipper
    Junctions (Junctions, getJunctions),

    -- ** Construction
    consLeft,
    consRight,
    paveJunctions,

    -- ** Representation
    DisjunctZipper (LeftBranch, RightBranch),
  )
where

import Language.Spectacle.Syntax.Modal.Term
  ( Term
      ( Always,
        Complement,
        Conjunct,
        Disjunct,
        Eventually,
        InfinitelyOften,
        StaysAs,
        UpUntil,
        Value
      ),
  )

-- ---------------------------------------------------------------------------------------------------------------------

-- | The 'Junctions' data type is a derivative type(or zipper) that indexes into disjunctions in a 'Term' expression.
--
-- [Example]
--
-- For any formula containing a disjunction, a set of 'Junctions' is constructed for all branches of disjunction in the
-- formula that could be satisfied to show that the whole formula has been satisfied.
--
-- @
-- formula :: Invariant ctx Bool
-- formula = always (p ∨ q) ∨ eventually p'
-- @
--
-- for example, would produce the following set of 'Junctions' with the correspondng satisfaction commitments:
--
-- @
-- [LeftBranch, LeftBranch] -- attempts to satisfy always p
-- [LeftBranch, RightBranch] -- attempts to satisfy always q
-- [RightBranch] -- attempts to satisfy eventually p'
-- @
--
-- @since 0.1.0.0
newtype Junctions = Junctions
  {getJunctions :: [DisjunctZipper]}
  deriving (Eq, Show)

-- | Enumeration of possible branches in a disjunction. For a disjunction @p ∨ q@ 'LeftBranch' corresponds to p and
-- 'RightBranch' to q.
--
-- @since 0.1.0.0
data DisjunctZipper
  = LeftBranch
  | RightBranch
  deriving (Eq, Enum, Show)

-- | @since 0.1.0.0
instance Semigroup Junctions where
  Junctions xs <> Junctions ys = Junctions (xs <> ys)
  {-# INLINE CONLIKE (<>) #-}

-- | @since 0.1.0.0
instance Monoid Junctions where
  mempty = Junctions []
  {-# INLINE CONLIKE mempty #-}

-- | Appends a 'LeftBranch' to the head of given 'Junctions'.
--
-- @since 0.1.0.0
consLeft :: Junctions -> Junctions
consLeft (Junctions xs) = Junctions (LeftBranch : xs)
{-# INLINE CONLIKE consLeft #-}

-- | Appends a 'RightBranch' to the head of a given 'Junctions'.
--
-- @since 0.1.0.0
consRight :: Junctions -> Junctions
consRight (Junctions xs) = Junctions (RightBranch : xs)
{-# INLINE CONLIKE consRight #-}

-- | Constructs a set of 'Junctions' where every element is unique root-to-leaf path with respect to disjunction
-- branches of the given 'Term' expression.
--
-- @since 0.1.0.0
paveJunctions :: Term a -> [Junctions]
paveJunctions = \case
  Value _ -> return mempty
  Disjunct e1 e2 ->
    let leftPath = fmap consLeft (paveJunctions e1)
        rightPath = fmap consRight (paveJunctions e2)
     in leftPath ++ rightPath
  Conjunct e1 e2 -> paveJunctions e1 ++ paveJunctions e2
  Complement e -> paveJunctions e
  Always _ e -> paveJunctions e
  Eventually _ e -> paveJunctions e
  UpUntil _ e1 e2 -> paveJunctions e1 ++ paveJunctions e2
  StaysAs _ e -> paveJunctions e
  InfinitelyOften _ e -> paveJunctions e
{-# INLINE paveJunctions #-}
