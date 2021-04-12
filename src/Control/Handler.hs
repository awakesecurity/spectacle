-- | The 'Handler' wrapper over weaving functions.
--
-- @since 0.1.0.0
module Control.Handler
  ( Handler (Handler, unHandler),
    identity,
    hoist,
    compose,
  )
where

import Control.Natural (type (~>))
import Data.Functor.Compose (Compose (..))
import Data.Functor.Identity (Identity (..))

-- -------------------------------------------------------------------------------------------------

-- | 'Handler' is a newtype wrapper over weaving functions. A 'Handler' almost always some form of:
--
-- @
-- higherOrderHandler :: Handler f (Lang ctx (eff ': effs) a) (Lang ctx effs a)
-- @
--
-- 'Handler' sometimes carries the handler for a first-order effect, in which case @f@ just becomes
-- 'Identity'.
--
-- @since 0.1.0.0
newtype Handler f m n = Handler
  {unHandler :: forall x. f (m x) -> n (f x)}

-- | Constructs the identity 'Handler'.
--
-- @
-- id == Handler . fmap Identity . runIdentity
-- @
--
-- @since 0.1.0.0
identity :: Functor m => Handler Identity m m
identity = Handler (fmap Identity . runIdentity)
{-# INLINE identity #-}

-- | Constructs a 'Handler' from a natural transformation.
--
-- @since 0.1.0.0
hoist :: Functor g => (f ~> g) -> Handler Identity f g
hoist eta = Handler (fmap Identity . eta . runIdentity)

-- | The composition of handlers.
--
-- @since 0.1.0.0
compose :: (Functor g, Functor o) => Handler f m n -> Handler g n o -> Handler (Compose g f) m o
compose (Handler eta) (Handler eta') = Handler (fmap Compose . eta' . fmap eta . getCompose)
