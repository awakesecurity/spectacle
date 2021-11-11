{-# LANGUAGE TupleSections #-}

-- | Effect queues.
--
-- @since 0.1.0.0
module Control.Applicative.Queue
  ( Queue (QU),
    pattern Queue,
    getQU,
    runQueue,
    getQueue,
    distribQueue,
    wrapQueue,
    cofreeFreeQueue,
    fixQ,
    now,
    later,
  )
where

import Control.Applicative (Alternative, Applicative, liftA2)
import Control.Monad.Zip (MonadZip)
import Data.Bifunctor
import Data.Kind (Type)

import Control.Applicative.Day (Day (Day), getDay)
import Control.Applicative.Phases (Phases (Here, There), hoist, lower)
import qualified Control.Applicative.Phases as Phases
import Control.Comonad.Cofree
import Control.Monad.Free.Ap

-- ---------------------------------------------------------------------------------------------------------------------

-- | Effect queues.
--
-- @since 0.1.0.0
newtype Queue :: (Type -> Type) -> Type -> Type where
  -- TODO: prove that 'Queue' is a (free) monad.
  QU :: {getQU :: Day (Phases f) a} -> Queue f a
  deriving (Functor)

pattern Queue :: (forall x. Phases f x -> Phases f (a, x)) -> Queue f a
pattern Queue {getQueue} = QU (Day getQueue)
{-# COMPLETE Queue #-}

-- | @since 0.1.0.0
instance (Applicative f, MonadZip g, Monoid a) => Semigroup (Queue f (g a)) where
  Queue f <> Queue g = Queue \x ->
    let here = Phases.tensor (fmap fst (f x)) (fmap fst (g x))
        there = fmap snd (f x)
     in liftA2 (,) here there

-- | @since 0.1.0.0
instance Applicative f => Applicative (Queue f) where
  pure x = Queue (fmap (x,))
  {-# INLINE pure #-}

  Queue f <*> Queue g = Queue \x ->
    let fx = f x
        gx = g (fmap snd fx)
     in liftA2 (,) (fmap fst fx <*> fmap fst gx) (fmap snd gx)
  {-# INLINE (<*>) #-}

runQueue :: Applicative f => Queue f a -> f a
runQueue = fmap fst . Phases.lower . flip getQueue (Here ())

fixQ :: Applicative f => (a -> Queue f a) -> Queue f a
fixQ f = Queue (getQueue (later (fixQ f)))

distribQueue :: Applicative f => f (Queue f a) -> Queue f (f a)
distribQueue ma = Queue \mx ->
  let mb = fmap (lower . fmap fst . (`getQueue` mx)) ma
   in case mx of
        Here x -> There (,) mb mx
        There f x xs -> There (\(e, a) b -> (e, f a b)) (liftA2 (,) mb x) xs

wrapQueue :: Monad f => f (Queue f a) -> Queue f a
wrapQueue f = Queue \x -> Phases.lift (fmap (($ x) . getQueue) f)

cofreeFreeQueue :: (Alternative f, Monad f) => Queue (Free f) a -> Queue f (Cofree f a)
cofreeFreeQueue (Queue f) = Queue \x -> case f (hoist liftF x) of
  Here (y, k) -> Here (pure y, k)
  There g y ys -> There (\a b -> first pure (g a b)) (retract y) (hoist retract ys)

now :: Applicative f => f a -> Queue f a
now xs = Queue \case
  Here x -> There (,) xs (Here x)
  There f y ys -> There (\(a, b) c -> (a, f b c)) (liftA2 (,) xs y) ys

later :: Applicative f => Queue f a -> Queue f a
later q = Queue (delay q)
  where
    delay :: Applicative f => Queue f a -> Phases f b -> Phases f (a, b)
    delay (Queue f) (Here y) = There (const id) (pure ()) (f (Here y))
    delay (Queue f) (There g y ys) = There (fmap . g) y (f ys)
