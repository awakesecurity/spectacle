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

-- | 'CheckResult' carries the information obtained by a checking model's invariant that is relevant for deciding if a
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
  {-# INLINE (>+<) #-}

-- | @since 0.1.0.0
instance Multiplicative CheckResult where
  unitOne = CheckResult True False
  {-# INLINE CONLIKE unitOne #-}

  leftResult >*< rightResult =
    CheckResult
      (leftResult ^. isSatisfied && rightResult ^. isSatisfied)
      (leftResult ^. isComplete && rightResult ^. isComplete)
  {-# INLINE (>*<) #-}

isSatisfied :: Lens' CheckResult Bool
isSatisfied = lens _isSatisfied \result x -> result {_isSatisfied = x}
{-# INLINE isSatisfied #-}

isComplete :: Lens' CheckResult Bool
isComplete = lens _isComplete \result x -> result {_isComplete = x}
{-# INLINE isComplete #-}
