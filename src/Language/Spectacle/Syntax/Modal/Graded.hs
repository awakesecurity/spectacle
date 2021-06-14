{-# LANGUAGE TypeFamilies #-}

module Language.Spectacle.Syntax.Modal.Graded
  ( -- * Syntactic Levels
    Level (L1, L2, L3, L4),
    levelMismatch,
    levelsAsNums,
    fromLevel,
    levelsOf,
    nameFromL3,

    -- ** Construction
    SyntaxLevel (fromPreterm),
    LTerm
      ( ValueL1,
        EmbedL1,
        ConjunctL2,
        DisjunctL2,
        ComplementL2,
        ImpliesL2,
        NotImpliesL2,
        AlwaysL3,
        EventuallyL3,
        UpUntilL3,
        InfinitelyOftenL3,
        StaysAsL3,
        ModalL4,
        ConjunctL4,
        DisjunctL4
      ),
  )
where

import Data.Kind (Constraint, Type)

import Language.Spectacle.Exception.RuntimeException (SyntaxException (ComplementInL3, LevelMismatch))
import Language.Spectacle.Syntax.Modal.Preterm
  ( Preterm
      ( PreAlways,
        PreComplement,
        PreConjunct,
        PreConst,
        PreDisjunct,
        PreUpUntil
      ),
    pattern PreEventually,
    pattern PreImplies,
    pattern PreNotImplies,
  )

-- ---------------------------------------------------------------------------------------------------------------------

-- | An enumeration of syntactic levels for temporal formula.
--
-- @since 0.1.0.0
data Level = L1 | L2 | L3 | L4

-- | The class of representations for well-formed temporal formula.
--
-- @since 0.1.0.0
type SyntaxLevel :: Level -> Constraint
class SyntaxLevel n where
  -- | 'LTerm' is a leveled (or graded) term. It is a 'Preterm' along with a type-level grading that constrains the
  -- structure of the AST such that only valid TLA formula are representable in 'LTerm'.
  --
  -- @since 0.1.0.0
  data LTerm n :: Type -> Type

  -- | Sends a possibly malformed 'Preterm' to an equivalent 'LTerm'.
  --
  -- @since 0.1.0.0
  fromPreterm :: Preterm Bool -> Either SyntaxException (LTerm n Bool)

-- | @since 0.1.0.0
instance SyntaxLevel 'L1 where
  data LTerm 'L1 a where
    ValueL1 :: a -> LTerm 'L1 a
    deriving (Show)

  fromPreterm (PreConst x) = return (ValueL1 x)
  fromPreterm term = Left (levelMismatch 0 term)
  {-# INLINE CONLIKE fromPreterm #-}

-- | @since 0.1.0.0
instance SyntaxLevel 'L2 where
  data LTerm 'L2 a where
    EmbedL1 :: LTerm 'L1 a -> LTerm 'L2 a
    ConjunctL2 :: LTerm 'L2 a -> LTerm 'L2 a -> LTerm 'L2 a
    DisjunctL2 :: LTerm 'L2 a -> LTerm 'L2 a -> LTerm 'L2 a
    ComplementL2 :: LTerm 'L2 a -> LTerm 'L2 a
    ImpliesL2 :: LTerm 'L2 a -> LTerm 'L3 a -> LTerm 'L2 a
    NotImpliesL2 :: LTerm 'L2 a -> LTerm 'L3 a -> LTerm 'L2 a
    deriving (Show)

  fromPreterm = \case
    PreConst x -> return (EmbedL1 (ValueL1 x))
    PreImplies lhs rhs
      | PreAlways {} <- rhs -> ImpliesL2 <$> fromPreterm lhs <*> fromPreterm rhs
      | PreUpUntil {} <- rhs -> ImpliesL2 <$> fromPreterm lhs <*> fromPreterm rhs
      | otherwise -> do
          lhs' <- fromPreterm lhs
          rhs' <- fromPreterm rhs
          return (DisjunctL2 (ComplementL2 lhs') rhs')
    PreNotImplies lhs rhs -> NotImpliesL2 <$> fromPreterm lhs <*> fromPreterm rhs
    PreConjunct lhs rhs -> ConjunctL2 <$> fromPreterm lhs <*> fromPreterm rhs
    PreDisjunct lhs rhs -> DisjunctL2 <$> fromPreterm lhs <*> fromPreterm rhs
    PreComplement terms -> ComplementL2 <$> fromPreterm terms
    terms -> Left (levelMismatch 2 terms)
  {-# INLINE fromPreterm #-}

-- | @since 0.1.0.0
instance SyntaxLevel 'L3 where
  data LTerm 'L3 a where
    AlwaysL3 :: Int -> LTerm 'L2 a -> LTerm 'L3 a
    EventuallyL3 :: Int -> LTerm 'L2 a -> LTerm 'L3 a
    UpUntilL3 :: Int -> LTerm 'L2 a -> LTerm 'L2 a -> LTerm 'L3 a
    InfinitelyOftenL3 :: Int -> LTerm 'L2 a -> LTerm 'L3 a
    StaysAsL3 :: Int -> LTerm 'L2 a -> LTerm 'L3 a
    deriving (Show)

  fromPreterm = \case
    PreComplement {} ->
      -- At the level of modal expressions complement/negation is valid; however, it should be factored by the dual laws
      -- and distributive laws when preterms are rewritten.
      Left ComplementInL3
    PreEventually name terms
      | PreAlways _ terms' <- terms -> StaysAsL3 name <$> fromPreterm @ 'L2 terms'
      | otherwise -> EventuallyL3 name <$> fromPreterm @ 'L2 terms
    PreUpUntil name lhs rhs -> UpUntilL3 name <$> fromPreterm @ 'L2 lhs <*> fromPreterm @ 'L2 rhs
    PreAlways name terms
      | PreEventually _ terms' <- terms -> InfinitelyOftenL3 name <$> fromPreterm @ 'L2 terms'
      | otherwise -> AlwaysL3 name <$> fromPreterm @ 'L2 terms
    terms -> Left (levelMismatch 3 terms)
  {-# INLINE fromPreterm #-}

-- | @since 0.1.0.0
instance SyntaxLevel 'L4 where
  data LTerm 'L4 a where
    ModalL4 :: LTerm 'L3 a -> LTerm 'L4 a
    ConjunctL4 :: LTerm 'L4 a -> LTerm 'L4 a -> LTerm 'L4 a
    DisjunctL4 :: LTerm 'L4 a -> LTerm 'L4 a -> LTerm 'L4 a
    deriving (Show)

  fromPreterm = \case
    terms@PreAlways {} -> ModalL4 <$> fromPreterm @ 'L3 terms
    terms@PreUpUntil {} -> ModalL4 <$> fromPreterm @ 'L3 terms
    PreConjunct lhs rhs -> ConjunctL4 <$> fromPreterm @ 'L4 lhs <*> fromPreterm @ 'L4 rhs
    PreDisjunct lhs rhs -> DisjunctL4 <$> fromPreterm @ 'L4 lhs <*> fromPreterm @ 'L4 rhs
    terms -> Left (levelMismatch 4 terms)
  {-# INLINE fromPreterm #-}

-- | Construct a level mismatch exception from the expected level and the term that is mismatched.
--
-- @since 0.1.0.0
levelMismatch :: Int -> Preterm a -> SyntaxException
levelMismatch level = LevelMismatch level . levelsAsNums
{-# INLINE levelMismatch #-}

-- | Get a list syntactic levels as 'Num's that the given 'Preterm' can occur at.
--
-- @since 0.1.0.0
levelsAsNums :: Num b => Preterm a -> [b]
levelsAsNums term = map fromLevel (levelsOf term)
{-# INLINE levelsAsNums #-}

-- | Get the equivalent 'Num' from a 'Level'.
--
-- @since 0.1.0.0
fromLevel :: Num a => Level -> a
fromLevel L1 = 1
fromLevel L2 = 2
fromLevel L3 = 3
fromLevel L4 = 4
{-# INLINE CONLIKE fromLevel #-}

-- | Get a list of syntactic levels the given term can occur.
--
-- @since 0.1.0.0
levelsOf :: Preterm a -> [Level]
levelsOf = \case
  PreConst {} -> [L1, L2]
  PreConjunct {} -> [L2, L4]
  PreDisjunct {} -> [L2, L4]
  PreComplement {} -> [L2, L3]
  PreImplies {} -> [L2]
  PreNotImplies {} -> [L2]
  PreAlways {} -> [L3]
  PreUpUntil {} -> [L3]
{-# INLINE CONLIKE levelsOf #-}

-- | Extract the unique identifier assigned to a 'LTerm' of 'L3'.
--
-- @since 0.1.0.0
nameFromL3 :: LTerm 'L3 a -> Int
nameFromL3 = \case
  AlwaysL3 name _ -> name
  EventuallyL3 name _ -> name
  UpUntilL3 name _ _ -> name
  InfinitelyOftenL3 name _ -> name
  StaysAsL3 name _ -> name
{-# INLINE nameFromL3 #-}
