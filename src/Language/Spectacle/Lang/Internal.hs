{-# LANGUAGE UndecidableInstances #-}

-- | The 'Lang' monad.
--
-- @since 0.1.0.0
module Language.Spectacle.Lang.Internal
  ( Lang (Pure, Yield),
    send,
    scope,
    Union (Op, Scoped),
  )
where

import Control.Applicative (Alternative (empty, (<|>)))
import Control.Monad (MonadPlus (mplus, mzero), (>=>))
import Data.Bool (bool)
import Data.Kind (Type)
import GHC.TypeLits (Symbol)

import Data.Ascript (Ascribe)
import Data.Functor.Loom (Loom)
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
-- @since 0.1.0.0
type Lang :: [Ascribe Symbol Type] -> [EffectK] -> Type -> Type
data Lang ctx effs a where
  Pure :: a -> Lang ctx effs a
  Yield :: Union ctx effs a -> (a -> Lang ctx effs b) -> Lang ctx effs b

-- | Sends a constructor for the effect @eff@ for 'Lang' to handle.
--
-- @since 0.1.0.0
send :: Member eff effs => eff a -> Lang ctx effs a
send eff = Yield (Op (inject eff)) pure
{-# INLINE send #-}

-- | Like 'send', but sends a constructor for the 'Effect' instance of @eff@.
--
-- @since 0.1.0.0
scope :: Member eff effs => Effect eff (Lang ctx effs) a -> Lang ctx effs a
scope eff = Yield (Scoped (injectS eff) Loom.identity) pure
{-# INLINE scope #-}

-- | @since 0.1.0.0
instance Functor (Lang ctx effs) where
  fmap f (Pure x) = Pure (f x)
  fmap f (Yield u k) = Yield u (fmap f . k)
  {-# INLINE fmap #-}

-- | @since 0.1.0.0
instance Applicative (Lang ctx effs) where
  pure = Pure
  {-# INLINE CONLIKE pure #-}

  Pure f <*> m = fmap f m
  Yield u k <*> m = Yield u ((<*> m) . k)
  {-# INLINE (<*>) #-}

-- | @since 0.1.0.0
instance Monad (Lang ctx effs) where
  Pure x >>= f = f x
  Yield u k >>= f = Yield u (k >=> f)
  {-# INLINE (>>=) #-}

-- | @since 0.1.0.0
instance Member NonDet effs => Alternative (Lang ctx effs) where
  empty = send Empty
  {-# INLINE empty #-}

  a <|> b = send Choose >>= bool b a
  {-# INLINE (<|>) #-}

-- | @since 0.1.0.0
instance Member NonDet effs => MonadPlus (Lang ctx effs) where
  mzero = empty
  {-# INLINE mzero #-}

  mplus = (<|>)
  {-# INLINE mplus #-}

-- ------------------------------------------------------------------------------------------------

-- | 'Union' joins the extensible sums for both 'Op' and 'Scoped'.
--
-- @since 0.1.0.0
data Union ctx effs a where
  Op :: Op effs a -> Union ctx effs a
  Scoped ::
    Scoped effs (Lang ctx effs') a ->
    Loom (Lang ctx effs') (Lang ctx effs) a b ->
    Union ctx effs b
