-- | An internal module hosting the datatypes comprising continuations for the
-- 'Language.Spectacle.Lang' machine.
--
-- @since 0.1.0.0
module Language.Spectacle.Lang.Control
  ( Control (..),
    Frame (..),
    pushSyntax,
    Store (..),
  )
where

import Data.Kind (Type)
import GHC.TypeLits (Symbol)

import Data.FTCQueue (FTCQueue (Leaf), pattern (:|>))
import Language.Spectacle.Lang.Sum (Sum, Syntax)
import Language.Spectacle.Type.Rec (Ascribe, Rec)

-- -----------------------------------------------------------------------------

-- | 'Control' implements continuations for 'Language.Spectacle.Lang' machines.
-- It is Oleg's "Freer" monad extended to support store passing of a
-- heterogeneously-typed record elements witnessed by the @cxt@ binder.
--
-- @since 0.1.0.0
type Control :: [Syntax] -> [Ascribe Symbol Type] -> Type -> Type
data Control sig cxt a where
  Done :: Store cxt a -> Control sig cxt a
  Next :: Frame sig cxt a -> Control sig cxt a

-- | @since 0.1.0.0
instance Functor (Control sig cxt) where
  fmap f (Done store) = Done (fmap f store)
  fmap f (Next frame) = Next (fmap f frame)
  {-# INLINE fmap #-}

-- -----------------------------------------------------------------------------

-- | 'Frame' is a continuation frame for a 'Lang' machine.
data Frame sig cxt a = forall x.
  Frame
  { handler :: Sum sig x
  , continue :: FTCQueue (Control sig) cxt x a
  }

-- | Constructs a 'Frame' with the provided effect and the identity
-- continuation.
--
-- @since 0.1.0.0
pushSyntax :: Sum sig a -> Frame sig cxt a
pushSyntax hdl = Frame hdl (Leaf \store x -> Done (Store store x))
{-# INLINE pushSyntax #-}

-- | @since 0.1.0.0
instance Functor (Frame sig cxt) where
  fmap f (Frame hdl q) = Frame hdl . (q :|>) $ \store x ->
    Done (Store store (f x))
  {-# INLINE fmap #-}

-- -----------------------------------------------------------------------------

-- | The type of stores for the spectacle 'Language.Spectacle.Lang.Lang'
-- machine.
--
-- @since 0.1.0.0
data Store cxt a = Store
  { stored :: Rec cxt
  , carried :: a
  }

-- | @since 0.1.0.0
instance Functor (Store cxt) where
  fmap f (Store st x) = Store st (f x)
  {-# INLINE fmap #-}
