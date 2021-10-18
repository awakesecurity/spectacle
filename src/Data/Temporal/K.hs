-- | Generalized temporal functor, capable of representing Global/Future modalities and transformations between them.
--
-- === Reference
--
-- 1. Temporal Logic with “Until”, Functional Reactive Programming with Processes, and Concrete Process Categories
--
-- @since 0.1.0.0
module Data.Temporal.K
  ( -- * K Functor
    K (InL, InR),
    pureK,
    inR,
    inL,
    unwrapK,
    toGL,
    toF,

    -- ** Universal Properties
    sumK,
    parK,
  )
where

import Control.Applicative (Alternative)
import Control.Comonad.Cofree (Cofree, ComonadCofree (unwrap))
import Data.Kind (Type)

import Control.Applicative.Queue (Queue, now, runQueue)
import Data.Temporal.Future (F (F), future, getF)
import Data.Temporal.Global (GL (GL), getGL)

-- ----------------------------------------------------------------------------------------------------------------------

data K :: (Type -> Type) -> Type -> Type where
  InL :: Queue f (Cofree f a) -> K f a
  InR :: Cofree f a -> K f a
  deriving (Functor)

pureK :: Alternative f => a -> K f a
pureK = InR . pure

-- | Inject a cofree stream into a global modality.
--
-- @since 0.1.0.0
inR :: Cofree f a -> K f a
inR = InR

-- | Inject a cofree stream into a future modality.
--
-- @since 0.1.0.0
inL :: Monad f => Cofree f a -> K f a
inL = InL . getF . future

-- | Unwrap a single step of 'K'.
--
-- @since 0.1.0.0
unwrapK :: Monad f => K f a -> f (K f a)
unwrapK (InL q) = runQueue (fmap (InL . now . unwrap) q)
unwrapK (InR w) = fmap inR (unwrap w)

-- | Extract 'K' into a global modality.
--
-- @since 0.1.0.0
toGL :: Applicative f => K f a -> f (GL f a)
toGL (InL q) = fmap GL (runQueue q)
toGL (InR w) = pure (GL w)

-- | Extract 'K' into a future modality.
--
-- @since 0.1.0.0
toF :: Monad f => K f a -> F f a
toF (InL q) = F q
toF (InR w) = future w

-- | 'sumK' is the span of some @a@. Provided a choice of morphisms @f :: a -> F f b@ and @g :: a -> GL f b@; we can
-- construct a unique morphism @a -> K f b@.
--
-- @since 0.1.0.0
sumK :: Either (a -> F f b) (a -> GL f b) -> a -> K f b
sumK (Left f) = InL . getF . f
sumK (Right g) = InR . getGL . g

-- | 'parK' is the cospawn of some @K f a@, provided two morphism @f :: F f a -> K f b@ and @g :: GL f a -> K f b@; we
-- can construct a unique morphism @K f a -> K f b@.
--
-- @since 0.1.0.0
parK :: K f a -> (F f a -> K f b) -> (GL f a -> K f b) -> K f b
parK (InL q) f _ = f (F q)
parK (InR w) _ g = g (GL w)
