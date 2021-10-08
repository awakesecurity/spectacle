{-# LANGUAGE GeneralizedNewtypeDeriving #-}

-- | Temporal monad defining eventually. Eventually is defined as a behavior leading up to a proof that the eventually
-- qualified formula holds, to represent this 'F' sums an effectful queue witnessing either a proof the formula holds
-- here or a continuation of next-behaviors that can be explored.
--
-- === Reference
--
-- 1. Towards a Common Categorical Semantics Linear-Time Temporal Logic Functional Reactive Programming
--
-- @since 0.1.0.0
module Data.Temporal.Future
  ( -- * Future/Eventually Modality
    F (F),
    getF,

    -- ** Natural transformations
    future,
    endoF,
    idealF,
  )
where

import Control.Applicative (Alternative, Applicative (liftA2))
import Control.Comonad.Cofree (Cofree ((:<)))
import Data.Kind (Type)

import Control.Applicative.Day (Day (Day, getDay))
import Control.Applicative.Phases (Phases (Here, There))
import Control.Applicative.Queue (Queue, later, now, wrapQueue)

-- ---------------------------------------------------------------------------------------------------------------------

newtype F :: (Type -> Type) -> Type -> Type where
  F :: {getF :: Queue f (Cofree f a)} -> F f a
  deriving (Functor)

-- | @since 0.1.0.0
instance (Applicative f, Alternative f) => Applicative (F f) where
  pure = F . pure . pure
  {-# INLINE pure #-}

  liftA2 f (F p) (F q) = F (liftA2 (liftA2 f) p q)
  {-# INLINE liftA2 #-}

-- | Lift a 'Cofree' stream into the future modality.
--
-- @since 0.1.0.0
future :: Monad f => Cofree f a -> F f a
future (t :< ts) =
  let here = pure t
      there = fmap pure (wrapQueue (fmap (getF . future) ts))
   in F (liftA2 (:<) (now here) (later there))

-- | 'endF' is the natural transformation defining the future modality. The transformation is defined by choosing when to
-- stop extracting from the inner 'Cofree' stream based on if the 'Queue' is 'Here' or 'There'. Witnessing 'Here' is
-- analogous to witnessing 'F' holds at the current point in the stream whereas 'There' witnesses that 'F' does not hold
-- at the current point in the stream.
--
-- @since 0.1.0.0
endoF :: Applicative f => F f a -> F f a
endoF (F q) = F $ Day \case
  Here x -> liftA2 ((,) . fst) (getDay q (Here x)) (Here x)
  There f x xs -> There (\a (b, c) -> (b, f a c)) x (getDay q xs)

-- | Ideal monad transform. Analogous to something like wrapping free transformed over either.
--
-- @since 0.1.0.0
idealF :: Monad f => f (F f a) -> F f a
idealF fx = F (wrapQueue (fmap getF fx))
