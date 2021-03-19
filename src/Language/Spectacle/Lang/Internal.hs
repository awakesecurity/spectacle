{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE UndecidableInstances #-}

-- | An internal module defining the 'Lang' machine and its instances.
--
-- @since 0.1.0.0
module Language.Spectacle.Lang.Internal
  ( -- * CEK Machine
    Lang (..),
    send,
    queueApp,
    queueCompose,

    -- * Internal Effects
    NonDet (..),
  )
where

import Control.Applicative (Alternative (empty, (<|>)))
import Data.Bool (bool)
import Data.Kind (Type)
import GHC.TypeLits (Symbol)

import Data.FTCQueue
  ( FTCQueue,
    ViewL (ViewL),
    viewl,
    (|><|),
    pattern (:<|),
    pattern (:|>),
  )
import Language.Spectacle.Lang.Control
  ( Control (Done, Next),
    Frame (Frame),
    Store (Store),
    pushSyntax,
  )
import Language.Spectacle.Lang.Sum (Syntax, inject, type (|>))
import Language.Spectacle.Type.Rec (Ascribe, Rec)

-- -----------------------------------------------------------------------------

-- | 'Lang' is a CESK machine which hosts a set of effect kinds that represent
-- spectacle syntax along with their interpreters assigning each syntactic unit
-- an operational semantics.
--
-- * @sig@ - is label set of effect kinds a 'Lang' machine holds.
--
-- * @cxt@ - is a context for typed variables in the scope of a 'Lang' machines
-- store.
--
-- * @a@ - is type of value resulting after all effects in the scope of @sig@
-- have been discharged.
--
-- @since 0.1.0.0
type Lang :: [Syntax] -> [Ascribe Symbol Type] -> Type -> Type
newtype Lang sig cxt a = Lang
  { unLang ::
      Rec cxt ->
      Control sig cxt a
  }

-- | 'send' pushes an identity continuation frame of the syntax @m@ into
-- 'Lang'.
--
-- @since 0.1.0.0
send :: m |> sig => m (Lang sig cxt) a -> Lang sig cxt a
send m = Lang . const . Next $ pushSyntax (inject m)
{-# INLINE send #-}

-- | Reduces the head of a queue of 'Lang' continuations and enqueues the tail
-- if it exists.
--
-- @since 0.1.0.0
queueApp :: FTCQueue (Control sig) cxt a b -> a -> Lang sig cxt b
queueApp q x = Lang \store -> case viewl q of
  ViewL f -> f store x
  f :<| t -> case f store x of
    Done (Store store' x') -> unLang (queueApp t x') store'
    Next (Frame hdl q') -> Next (Frame hdl (q' |><| t))
{-# INLINE queueApp #-}

-- | Builds a new 'Lang' by fully evaluating the current continuation.
--
-- @since 0.1.0.0
queueCompose ::
  FTCQueue (Control sig) cxt a b ->
  (Lang sig cxt b -> Lang sig' cxt c) ->
  (a -> Lang sig' cxt c)
queueCompose g h = h . queueApp g
{-# INLINE queueCompose #-}

-- | @since 0.1.0.0
instance Functor (Lang sig cxt) where
  fmap f (Lang m) = Lang (fmap f . m)
  {-# INLINE fmap #-}

-- | @since 0.1.0.0
instance Applicative (Lang sig cxt) where
  pure x = Lang \store -> Done (Store store x)
  {-# INLINE pure #-}

  Lang m <*> Lang k = Lang \store -> case m store of
    Done (Store store' f) -> fmap f (k store')
    Next (Frame hdl q) -> Next (Frame hdl (q :|> \store' -> (`fmap` k store')))
  {-# INLINE (<*>) #-}

-- | @since 0.1.0.0
instance Monad (Lang sig cxt) where
  Lang m >>= k = Lang \store -> case m store of
    Done (Store store' x) -> unLang (k x) store'
    Next (Frame hdl q) ->
      Next (Frame hdl (q :|> \store' x -> unLang (k x) store'))
  {-# INLINE (>>=) #-}

-- -----------------------------------------------------------------------------

-- | Nondeterministic action.
--
-- @since 0.1.0.0
data NonDet :: Syntax where
  Empty :: NonDet m a
  Choose :: NonDet m Bool

-- | @since 0.1.0.0
instance NonDet |> sig => Alternative (Lang sig cxt) where
  empty = send Empty
  {-# INLINE empty #-}

  a <|> b = send Choose >>= bool b a
  {-# INLINE (<|>) #-}
