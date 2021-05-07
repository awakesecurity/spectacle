-- | The 'NonDet' effect models nondeterminism.
--
-- @since 0.1.0.0
module Language.Spectacle.Syntax.NonDet
  ( NonDet (Choose, Empty),
    Effect (NonDet),
    oneOf,
    foldMapA,
    runNonDetA,
  )
where

import Control.Applicative (Alternative (empty, (<|>)), Applicative (liftA2))
import Data.Coerce (coerce)
import Data.Monoid (Alt (Alt, getAlt))
import Data.Void (absurd)

import Data.Functor.Loom ( (~>~), hoist )
import Language.Spectacle.Lang
  ( Effect,
    Lang (Op, Pure, Scoped),
    decomposeOp,
    decomposeS,
  )
import Language.Spectacle.Syntax.NonDet.Internal
  ( Effect (NonDet),
    NonDet (Choose, Empty),
  )

-- -------------------------------------------------------------------------------------------------

-- | Nondeterministically choose an element from a foldable container.
--
-- @since 0.1.0.0
oneOf :: (Foldable t, Alternative m) => t a -> m a
oneOf = foldMapA pure

-- | Like `foldMap`, but folds under 'Alternative' rather than 'Monoid'.
--
-- @since 0.1.0.0
foldMapA :: (Foldable t, Alternative m) => (a -> m b) -> t a -> m b
foldMapA f = getAlt . foldMap (Alt . f)

-- | Discharge a 'NonDet' effect into some 'Alternative' functor @f@.
--
-- @since 0.1.0.0
runNonDetA :: Alternative f => Lang ctx (NonDet ': effs) a -> Lang ctx effs (f a)
runNonDetA = \case
  Pure x -> pure (pure x)
  Op op k -> case decomposeOp op of
    Left other -> Op other k'
    Right Empty -> pure empty
    Right Choose -> liftA2 (<|>) (k' True) (k' False)
    where
      k' = runNonDetA . k
  Scoped scoped loom -> case decomposeS scoped of
    Left other -> Scoped other (loom ~>~ hoist runNonDetA)
    Right bot -> absurd (coerce bot)
