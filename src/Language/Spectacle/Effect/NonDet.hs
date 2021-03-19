-- | Nondeterministic effects.
--
-- @since 0.1.0.0
module Language.Spectacle.Effect.NonDet
  ( -- * The NonDet effect
    NonDet,
    oneOf,
    foldMapA,

    -- ** Running NonDet
    runNonDetAll,
  )
where

import Control.Applicative (Alternative (empty), liftA2, (<|>))
import Data.Monoid (Alt (Alt, getAlt))

import Language.Spectacle.Lang (Lang, handleRelay)
import Language.Spectacle.Lang.Internal (NonDet (Choose, Empty))

-- -----------------------------------------------------------------------------

-- | Nondeterministically choose an element @a@ from a foldable container @t a@.
-- This emulates list-style nondeterministic choice.
--
-- @since 0.1.0.0
oneOf :: (Foldable t, Alternative m) => t a -> m a
oneOf = foldMapA pure
{-# INLINE oneOf #-}

-- | Like 'foldMap', but folds using an 'Alternative' instance rather than a
-- 'Monoid'.
--
-- @since 0.1.0.0
foldMapA :: (Foldable t, Alternative m) => (a -> m b) -> t a -> m b
foldMapA f = getAlt . foldMap (Alt . f)
{-# INLINE foldMapA #-}

-- | Collect the results of a nondeterministic computation into an 'Alternative'
-- functor @f@.
--
-- @since 0.1.0.0
runNonDetAll :: Alternative f => Lang (NonDet ': sig) cxt a -> Lang sig cxt (f a)
runNonDetAll = handleRelay (pure . pure) \_ label k -> case label of
  Empty -> pure empty
  Choose -> liftA2 (<|>) (k True) (k False)
{-# INLINE runNonDetAll #-}
