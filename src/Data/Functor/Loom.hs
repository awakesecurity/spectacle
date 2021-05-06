-- | The 'Loom' functor. The mnemonic for 'Loom' is from it accumulating a continuation of /weaving/
-- functions.
--
-- @since 0.1.0.0
module Data.Functor.Loom
  ( -- * Loom
    Loom (Loom),
    runLoom,
    (~>~),
    identity,

    -- * Loom Combinators
    weave,
    bind,
    lift,
    hoist,
  )
where

import Control.Monad ((>=>))
import Data.Functor.Compose (Compose (Compose, getCompose))
import Data.Functor.Identity (Identity (Identity, runIdentity))

-- ---------------------------------------------------------------------------------------------------------------------

-- | 'Loom' is very similar to Coyoneda but accumulates weaving functions rather than ordinary functions. 'Loom' is used
-- to build up a continuation of higher-order effect handlers.
--
-- @since 0.1.0.0
data Loom m n a b where
  Loom :: Functor f => f () -> (f (m a) -> n b) -> Loom m n a b

-- | @since 0.1.0.0
instance Functor n => Functor (Loom m n a) where
  fmap f (Loom ctx eta) = Loom ctx (fmap f . eta)
  {-# INLINE fmap #-}

-- | Unwraps a 'Loom' into a natural transformation.
--
-- @since 0.1.0.0
runLoom :: Loom m n a b -> (m a -> n b)
runLoom (Loom ctx eta) m = eta (m <$ ctx)
{-# INLINE runLoom #-}

-- | 'Loom' composition, 'Loom's can be constructed with the 'bind', 'lift', 'run', and 'hoist' combinators
-- and subsequently composed to produce complex weaves in a straightforward way:
--
-- @
-- -- weaving a state
-- let loom :: Loom (Lang ctx effs') (Lang ctx effs) a b
-- let weaveF :: s -> Lang ctx (eff ': effs) a -> Lang ctx effs (s, a)
--     weaveF = uncurry (loom ~>~ weave (st :: s, ()) (f :: s -> a -> Lang ctx effs (s a)))
-- @
--
-- @since 0.1.0.0
infixr 9 ~>~

(~>~) :: Loom m n a b -> Loom n o b c -> Loom m o a c
Loom ctx eta ~>~ Loom ctx' eta' = Loom (Compose (ctx <$ ctx')) (eta' . fmap eta . getCompose)
{-# INLINE (~>~) #-}

-- | Constructs the identity 'Loom'.
--
-- @
-- id == Loom (Identity ()) runIdentity
-- @
--
-- @since 0.1.0.0
identity :: Loom m m a a
identity = Loom (Identity ()) runIdentity
{-# INLINE identity #-}

-- | Constructs an "Effect Handlers in Scope"-style weaving function from the functor context and the distribution
-- function. This is a synonym for the 'Loom' constructor.
--
-- @since 0.1.0.0
weave :: Functor f => f () -> (f (m a) -> n (f b)) -> Loom m n a (f b)
weave = Loom
{-# INLINE weave #-}

-- | Constructs a 'Loom' from a bind function.
--
-- @since 0.1.0.0
bind :: Monad n => (a -> n b) -> Loom n n a b
bind k = Loom (Identity ()) (runIdentity >=> k)
{-# INLINE bind #-}

-- | Lifts a function to a 'Loom'. This is equivalent to:
--
-- @
-- 'hoist' ('fmap' f)
-- @
--
-- @since 0.1.0.0
lift :: Functor m => (a -> b) -> Loom m m a b
lift f = Loom (Identity ()) (fmap f . runIdentity)
{-# INLINE lift #-}

-- | Constructs a 'Loom' from a natural transformation.
--
-- @since 0.1.0.0
hoist :: (f a -> g b) -> Loom f g a b
hoist eta = Loom (Identity ()) (eta . runIdentity)
{-# INLINE hoist #-}
