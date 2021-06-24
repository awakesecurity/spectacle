module Language.Spectacle.Spec.CheckResult
  ( -- * Algebra
    Multiplicative (unitOne, (>*<)),
    Additive (unitZero, (>+<)),

    -- * Checker Results
    CheckResult (CheckResult, _isSatisfied, _isComplete),

    -- ** Lenses
    isSatisfied,
    isComplete,
  )
where

import Lens.Micro (Lens', lens, (^.))


-- ---------------------------------------------------------------------------------------------------------------------

-- | An instance of 'Multiplicative' for @a@ is the multiplicative monoid for @a@.
--
-- @since 0.1.0.0
class Multiplicative a where
  -- | The identity element under ('>*<').
  --
  -- @since 0.1.0.0
  unitOne :: a

  -- | A multiplicative operation on @a@.
  --
  -- @since 0.1.0.0
  infixl 7 >*<

  (>*<) :: a -> a -> a

-- | An instance of 'Additive' for @a@ is the additive monoid for @a@.
--
-- @since 0.1.0.0
class Additive a where
  -- | The identity element under ('>+<').
  --
  -- @since 0.1.0.0
  unitZero :: a

  -- | The additive operation on @a@.
  --
  -- @since 0.1.0.0
  infixl 6 >+<

  (>+<) :: a -> a -> a

-- ---------------------------------------------------------------------------------------------------------------------

-- | 'CheckResult' carries the information obtained by checking model's invariant that is relevant for deciding if a
-- model checking should continue.
--
-- @since 0.1.0.0
data CheckResult = CheckResult
  { -- | Boolean field asserting if the temporal formula was satisfied or not.
    --
    -- @since 0.1.0.0
    _isSatisfied :: Bool
  , -- | '_isComplete' indicates if all temporal properties are known to be satisfied for a given world, i.e. if formula
    -- contains @'eventually' p == 'False'@ (or 'upUntil') then @'_isComplete' == 'False'@.
    --
    -- @since 0.1.0.0
    _isComplete :: Bool
  }
  deriving (Show)

-- | @since 0.1.0.0
instance Additive CheckResult where
  unitZero = CheckResult False False
  {-# INLINE CONLIKE unitZero #-}

  leftResult >+< rightResult =
    CheckResult
      (leftResult ^. isSatisfied || rightResult ^. isSatisfied)
      (leftResult ^. isComplete && rightResult ^. isComplete)
  -- ((leftResult ^. impliedFormula) >+< (rightResult ^. impliedFormula))
  {-# INLINE (>+<) #-}

-- | @since 0.1.0.0
instance Multiplicative CheckResult where
  unitOne = CheckResult True False
  {-# INLINE CONLIKE unitOne #-}

  leftResult >*< rightResult =
    CheckResult
      (leftResult ^. isSatisfied && rightResult ^. isSatisfied)
      (leftResult ^. isComplete && rightResult ^. isComplete)
  -- ((leftResult ^. impliedFormula) >*< (rightResult ^. impliedFormula))
  {-# INLINE (>*<) #-}

-- | @since 0.1.0.0
-- instance HasImpliedFormula CheckResult ImplicationTree where
--   impliedFormula = lens _impliedFormula \result x -> result {_impliedFormula = x}
--   {\-# INLINE impliedFormula #-\}
isSatisfied :: Lens' CheckResult Bool
isSatisfied = lens _isSatisfied \result x -> result {_isSatisfied = x}
{-# INLINE isSatisfied #-}

isComplete :: Lens' CheckResult Bool
isComplete = lens _isComplete \result x -> result {_isComplete = x}
{-# INLINE isComplete #-}

-- ---------------------------------------------------------------------------------------------------------------------

-- | 'ImplicationTree' is a binary tree in which leafs are a 'Set' of 'Implication's under conjunction and branches are
-- implied temporal under disjunction.
--
-- To show how this is used, assume state predicates @p_1@, @p_2@, @q_1@ and @q_2@ along with the temporal formula
--
-- @
-- p_1 ==> always q_1 \/ p_2 ==> eventually q_2
-- @
--
-- Assuming @p_1@ and @p_2@ are both known to be true, the model checker will construct the following 'ImplicationTree'
-- to represent the disjunction of implications while checking the invariant
--
-- @
-- 'ImpliedOr'
--   (Implied (Implication 0 ModalityAlways True))
--   (Implied (Implication 1 ModlaityEventually True))
-- @
--
-- The 'Additive'/'Multiplicative' instances for 'ImplicationTree' are respectively homomorphic to logical disjunction
-- and conjunction. So, the model checker can add 'ImplicationTree's for any disjunction and multiply for any
-- conjunctions. Finally, an 'ImplicationTree' can be flattened into a disjunction of zero or more properties, each of
-- which could be satisfied to show that the original formula is true.
--
-- @since 0.1.0.0
-- data ImplicationTree
--   = Implied (Set Implication)
--   | ImpliedOr ImplicationTree ImplicationTree
--   deriving (Show)

-- | Flattens a 'Implication' tree into a disjunction of many @'Set' 'Implication'@.
--
-- @since 0.1.0.0
-- flattenImplicationTree :: ImplicationTree -> [Set Implication]
-- flattenImplicationTree impl = nub (flatten impl)
--   where
--     flatten :: ImplicationTree -> [Set Implication]
--     flatten = \case
--       Implied xs -> return xs
--       ImpliedOr lhs rhs -> flattenImplicationTree lhs <> flattenImplicationTree rhs
-- {\-# INLINE flattenImplicationTree #-\}

-- | The addition of 'ImplicationTree's behaves like 'max' or ('||').
--
-- +---+---+---+
-- | + | 0 | 1 |
-- +---+---+---+
-- | 0 | 0 | 1 |
-- +---+---+---+
-- | 1 | 1 | 1 |
-- +---+---+---+
--
-- @since 0.1.0.0
-- instance Additive ImplicationTree where
--   unitZero = Implied Set.empty
--   {\-# INLINE CONLIKE unitZero #-\}

--   (>+<) = ImpliedOr
--   {-# INLINE CONLIKE (>+<) #-}

-- | The multiplication of 'ImplicationTree's behaves like 'min' or ('&&').
--
-- +---+---+---+
-- | + | 0 | 1 |
-- +---+---+---+
-- | 0 | 0 | 1 |
-- +---+---+---+
-- | 1 | 1 | 0 |
-- +---+---+---+
--
-- @since 0.1.0.0
-- instance Multiplicative ImplicationTree where
--   unitOne = Implied Set.empty
--   {\-# INLINE CONLIKE unitOne #-\}

--   Implied xs >*< Implied ys = Implied (xs <> ys)
--   xs >*< ImpliedOr lefts rights = ImpliedOr (xs >*< lefts) (xs >*< rights)
--   ImpliedOr lefts rights >*< xs = ImpliedOr (lefts >*< xs) (rights >*< xs)
--   {-# INLINE (>*<) #-}
