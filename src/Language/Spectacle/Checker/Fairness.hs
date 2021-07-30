-- | Process fairness constraints that can be specified to the model checker.
--
-- @since 0.1.0.0
module Language.Spectacle.Checker.Fairness
  ( -- * Fairness
    Fairness (Unfair, WeakFair, StrongFair),
    unfair,
    weakFair,
    strongFair,
  )
where

-- ---------------------------------------------------------------------------------------------------------------------

-- | An enumeration of process fairness types.
--
-- @since 0.1.0.0
data Fairness
  = Unfair
  | WeakFair
  | StrongFair
  deriving (Eq, Ord, Enum, Show)

unfair :: Fairness
unfair = Unfair
{-# INLINE CONLIKE unfair #-}

weakFair :: Fairness
weakFair = WeakFair
{-# INLINE CONLIKE weakFair #-}

strongFair :: Fairness
strongFair = StrongFair
{-# INLINE CONLIKE strongFair #-}
