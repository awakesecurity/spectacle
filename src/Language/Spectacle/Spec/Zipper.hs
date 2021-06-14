module Language.Spectacle.Spec.Zipper
  ( -- * Disjunction Zipper
    Junctions (Junctions, getJunctions),

    -- ** Construction
    consLeft,
    consRight,
    zipJunctions,
    paveJunctions,

    -- ** Representation
    DisjunctZipper (LeftBranch, RightBranch),
  )
where

import Data.Type.Rec (Rec)
import Language.Spectacle.Syntax.Modal.Graded
  ( LTerm (ConjunctL4, DisjunctL4, ModalL4),
    Level (L4),
  )

-- ---------------------------------------------------------------------------------------------------------------------

-- | 'Junctions' is a ordered list of 'DisjunctZipper'.
--
-- @since 0.1.0.0
newtype Junctions = Junctions
  {getJunctions :: [DisjunctZipper]}
  deriving (Eq, Show)

-- | @since 0.1.0.0
instance Semigroup Junctions where
  Junctions xs <> Junctions ys = Junctions (xs <> ys)
  {-# INLINE CONLIKE (<>) #-}

-- | @since 0.1.0.0
instance Monoid Junctions where
  mempty = Junctions []
  {-# INLINE CONLIKE mempty #-}

-- | Cons a left branch onto a 'Junctions'.
--
-- @since 0.1.0.0
consLeft :: Junctions -> Junctions
consLeft (Junctions xs) = Junctions (LeftBranch : xs)
{-# INLINE CONLIKE consLeft #-}

-- | Cons a right branch onto a 'Junctions'.
--
-- @since 0.1.0.0
consRight :: Junctions -> Junctions
consRight (Junctions xs) = Junctions (RightBranch : xs)
{-# INLINE CONLIKE consRight #-}

-- | Prepares an initial world with the set of possible 'Junctions', given by 'paveJunctions'.
--
-- @since 0.1.0.0
zipJunctions :: Rec ctx -> LTerm 'L4 a -> [(Rec ctx, Junctions)]
zipJunctions world formula = do
  junction <- paveJunctions formula
  return (world, junction)
{-# INLINE zipJunctions #-}

-- | Traverses an 'LTerm' for all 'L4' nodes and returns the corresponding set of possible 'Junctions' which can be
-- taken for terms in the tree.
--
-- @since 0.1.0.0
paveJunctions :: LTerm 'L4 a -> [Junctions]
paveJunctions = \case
  ModalL4 {} -> return mempty
  ConjunctL4 lhs rhs -> paveJunctions lhs <> paveJunctions rhs
  DisjunctL4 lhs rhs ->
    let leftPath = fmap consLeft (paveJunctions lhs)
        rightPath = fmap consRight (paveJunctions rhs)
     in leftPath <> rightPath
{-# INLINE paveJunctions #-}

-- | Zipper representation for level 4 (modal) disjunctions.
--
-- @since 0.1.0.0
data DisjunctZipper
  = LeftBranch
  | RightBranch
  deriving (Eq, Enum, Show)
