-- | The 'Loom' functor. The mnemonic for 'Loom' is from it accumulating a continuation of /weaving/
-- functions.
--
-- @since 0.1.0.0
module Data.Functor.Loom
  ( Loom (Loom),
    runLoom,
    weave,
    (~>~),
    identity,
    hoist,
  )
where

import Control.Natural (type (~>))
import Data.Functor.Compose (Compose (Compose, getCompose))
import Data.Functor.Identity (Identity (Identity, runIdentity))

import Control.Handler (Handler (Handler))
import qualified Control.Handler as Handler

-- -------------------------------------------------------------------------------------------------

-- | 'Loom' is very similar to Coyoneda but accumulates weaves as 'Handler' rather than ordinary
-- functions. 'Loom' is used to build up a continuation of higher-order handlers.
--
-- The @f ()@ is a functor that provides the structure for weaving, this is 'Identity' in the case
-- of purely first-order effects. For the higher-order @Error@ effect, @f@ becomes 'Either'.
--
-- The @(f a -> b)@ is an evaluation map. For purely first-order effects this is just 'runIdentity'.
-- Even in most cases for higher-order effects the evaluation map is only 'id'; however, there are
-- instances where being able to unpack the structure of @f@ after weaving is useful.
--
-- @since 0.1.0.0
data Loom m n a b where
  Loom :: Functor f => f () -> Handler f m n -> (f a -> b) -> Loom m n a b

-- | @since 0.1.0.0
instance Functor (Loom m n a) where
  fmap f (Loom ctx eta tmorphism) = Loom ctx eta (f . tmorphism)
  {-# INLINE fmap #-}

-- | Unwraps a 'Loom' into a natural transformation.
--
-- @since 0.1.0.0
runLoom :: forall m n a b. Functor n => Loom m n a b -> (m a -> n b)
runLoom (Loom ctx (Handler eta) tmorphism) m = fmap tmorphism (weaveEff ctx m eta)
  where
    weaveEff :: Functor f => f () -> m a -> (forall x. f (m x) -> n (f x)) -> n (f a)
    weaveEff structure eff distribute = distribute (eff <$ structure)
{-# INLINE runLoom #-}

-- | Composition of weaving functions with 'Loom'.
--
-- @since 0.1.0.0
weave ::
  (Functor f, Functor o) =>
  f () ->
  (forall x. f (n x) -> o (f x)) ->
  Loom m n a b ->
  Loom m o a (f b)
weave ctx eta (Loom ctx' eta' tmorphism) =
  Loom (Compose (ctx' <$ ctx)) (Handler.compose eta' (Handler eta)) (fmap tmorphism . getCompose)
{-# INLINE weave #-}

-- | Composition of 'Loom'.
--
-- @since 0.1.0.0
infixr 9 ~>~

(~>~) :: Functor o => Loom m n a b -> Loom n o b c -> Loom m o a c
Loom ctx eta tmorph ~>~ Loom ctx' eta' tmorph' =
  Loom (Compose (ctx <$ ctx')) (Handler.compose eta eta') (tmorph' . fmap tmorph . getCompose)
{-# INLINE (~>~) #-}

-- | Constructs the identity 'Loom'.
--
-- @
-- id == Loom (Identity ()) (fmap Identity . runIdentity) runIdentity)
-- @
--
-- @since 0.1.0.0
identity :: Functor m => Loom m m a a
identity = Loom (Identity ()) Handler.identity runIdentity
{-# INLINE identity #-}

-- | Constructs a 'Loom' from a natural transformation.
--
-- @since 0.1.0.0
hoist :: Functor g => (f ~> g) -> Loom f g a a
hoist eta = Loom (Identity ()) (Handler.hoist eta) runIdentity
{-# INLINE hoist #-}
