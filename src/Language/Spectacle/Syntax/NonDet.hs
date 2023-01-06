{-# OPTIONS_HADDOCK show-extensions #-}

-- |
-- Module      :  Language.Spectacle.Syntax.NonDet
-- Copyright   :  (c) Arista Networks, 2022-2023
-- License     :  Apache License 2.0, see LICENSE
--
-- Stability   :  stable
-- Portability :  non-portable (GHC extensions)
--
-- The 'NonDet' effect models nondeterminism.
--
-- @since 1.0.0
module Language.Spectacle.Syntax.NonDet
  ( NonDet (Choose, Empty),
    Effect (MSplit),
    oneOf,
    foldMapA,
    msplit,
    runNonDetA,
  )
where

import Control.Applicative (Alternative (empty, (<|>)), Applicative (liftA2))
import Data.Foldable (msum)
import Data.Monoid (Alt (Alt, getAlt))

import Data.Functor.Loom (hoist, runLoom, (~>~))
import Language.Spectacle.Lang
  ( Effect,
    Lang (Op, Pure, Scoped),
    Member (project),
    decomposeOp,
    decomposeS,
    scope,
  )
import Language.Spectacle.Syntax.NonDet.Internal (Effect (MSplit), NonDet (Choose, Empty))

-- ---------------------------------------------------------------------------------------------------------------------

-- | Nondeterministically choose an element from a foldable container.
--
-- @since 1.0.0
oneOf :: (Foldable t, Alternative m) => t a -> m a
oneOf = foldMapA pure
{-# INLINE oneOf #-}

-- | Like `foldMap`, but folds under 'Alternative' rather than 'Monoid'.
--
-- @since 1.0.0
foldMapA :: (Foldable t, Alternative m) => (a -> m b) -> t a -> m b
foldMapA f = getAlt . foldMap (Alt . f)
{-# INLINE foldMapA #-}

-- | Splits a 'Lang' into its 'Alternative' branches, if it has any.
--
-- @since 1.0.0
msplit :: Member NonDet effs => Lang ctx effs a -> Lang ctx effs (Maybe (Lang ctx effs a, Lang ctx effs a))
msplit m = scope (MSplit m)
{-# INLINE msplit #-}

-- | Discharge a 'NonDet' effect into some 'Alternative' functor @f@.
--
-- @since 1.0.0
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
    Right (MSplit m) -> runLoom (loom ~>~ hoist runNonDetA) (handleMSplit m)
    where
      handleMSplit :: Member NonDet effs => Lang ctx effs a -> Lang ctx effs (Maybe (Lang ctx effs a, Lang ctx effs a))
      handleMSplit = loop []
  where
    loop xs (Pure x) = pure (Just (pure x, msum xs))
    loop xs (Op op k) = case project op of
      Nothing -> Op op (loop xs . k)
      Just Empty -> case xs of
        [] -> pure Nothing
        (x : xs') -> loop xs' x
      Just Choose -> loop (k False : xs) (k True)
    loop xs (Scoped scoped loom) = Scoped scoped (loom ~>~ hoist (loop xs))
