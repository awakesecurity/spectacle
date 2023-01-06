{-# LANGUAGE UndecidableInstances #-}

-- |
-- Module      :  Language.Spectacle.Lang.Internal
-- Copyright   :  (c) Arista Networks, 2022-2023
-- License     :  Apache License 2.0, see LICENSE
--
-- Stability   :  stable
-- Portability :  non-portable (GHC extensions)
--
-- The 'Lang' monad.
--
-- @since 1.0.0
module Language.Spectacle.Lang.Internal
  ( Lang (Pure, Op, Scoped),
    send,
    scope,
  )
where

import Control.Applicative (Alternative (empty, (<|>)))
import Control.Monad (MonadPlus (mplus, mzero), (>=>))
import Data.Bool (bool)
import Data.Kind (Type)
import GHC.TypeLits (Symbol)

import Data.Ascript (Ascribe)
import Data.Functor.Loom (Loom (Loom), bind, (~>~))
import qualified Data.Functor.Loom as Loom
import Language.Spectacle.Lang.Member (Member (inject, injectS))
import Language.Spectacle.Lang.Op (Op)
import Language.Spectacle.Lang.Scoped (Effect, EffectK, Scoped)
import Language.Spectacle.Syntax.NonDet.Internal (NonDet (Choose, Empty))

-- -------------------------------------------------------------------------------------------------

-- | 'Lang' is a CEK-style interpreter for the set of effects behind Spectacles syntax and is based
-- on Oleg's Eff monad. 'Lang' differs from Eff in it's @ctx@ parameter and its ability to support
-- higher-order effects.
--
-- * The type parameter @ctx@ is a type row associating variable names to their respective types.
-- This includes plain values from the previous frame as well as primed variables in the next frame.
--
-- * The type parameter @effs@ is the set of effects a 'Lang' is capable of performing.
--
-- @since 1.0.0
type Lang :: [Ascribe Symbol Type] -> [EffectK] -> Type -> Type
data Lang ctxt effs a where
  Pure ::
    a ->
    Lang ctxt effs a
  Op ::
    Op effs a ->
    (a -> Lang ctxt effs b) ->
    Lang ctxt effs b
  Scoped ::
    Scoped effs (Lang ctxt effs') a ->
    Loom (Lang ctxt effs') (Lang ctxt effs) a b ->
    Lang ctxt effs b

-- | Sends a constructor for the effect @eff@ for 'Lang' to handle.
--
-- @since 1.0.0
send :: Member eff effs => eff a -> Lang ctx effs a
send eff = Op (inject eff) pure
{-# INLINE send #-}

-- | Like 'send', but sends a constructor for the 'Effect' instance of @eff@.
--
-- @since 1.0.0
scope :: Member eff effs => Effect eff (Lang ctx effs) a -> Lang ctx effs a
scope eff = Scoped (injectS eff) Loom.identity
{-# INLINE scope #-}

-- | @since 1.0.0
instance Functor (Lang ctx effs) where
  fmap f (Pure x) = Pure (f x)
  fmap f (Op u k) = Op u (fmap f . k)
  fmap f (Scoped u loom) = Scoped u (fmap f loom)
  {-# INLINE fmap #-}

-- | @since 1.0.0
instance Applicative (Lang ctx effs) where
  pure = Pure
  {-# INLINE CONLIKE pure #-}

  Pure f <*> m = fmap f m
  Op u k <*> m = Op u ((<*> m) . k)
  Scoped u (Loom ctx eta) <*> m = Scoped u (Loom ctx ((<*> m) . eta))
  {-# INLINE (<*>) #-}

-- | @since 1.0.0
instance Monad (Lang ctx effs) where
  Pure x >>= f = f x
  Op u k >>= f = Op u (k >=> f)
  Scoped u loom >>= f = Scoped u (loom ~>~ bind f)
  {-# INLINE (>>=) #-}

-- | @since 1.0.0
instance Member NonDet effs => Alternative (Lang ctx effs) where
  empty = send Empty
  {-# INLINE empty #-}

  a <|> b = send Choose >>= bool b a
  {-# INLINE (<|>) #-}

-- | @since 1.0.0
instance Member NonDet effs => MonadPlus (Lang ctx effs) where
  mzero = empty
  {-# INLINE mzero #-}

  mplus = (<|>)
  {-# INLINE mplus #-}
