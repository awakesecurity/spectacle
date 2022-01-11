{-# LANGUAGE TupleSections #-}

-- | Effect queues.
--
-- @since 0.1.0.0
module Control.Applicative.Queue
  ( -- * Queue
    Queue,
    runQueue,
    runQueueReverse,

    -- ** Lifting
    distribQueue,
    wrapQueue,

    -- ** Scheduling
    now,
    later,
    delay,
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
type Queue :: (Type -> Type) -> Type -> Type
type Queue f = Day (Phases f)

runQueue :: Applicative f => Queue f a -> f a
runQueue = fmap fst . Phases.lower . flip getDay (Here ())

runQueueReverse :: Applicative f => Queue f a -> f a
runQueueReverse = fmap fst . Phases.lowerR . flip getDay (Here ())

distribQueue :: Applicative f => f (Queue f a) -> Queue f (f a)
distribQueue ma = Day \mx ->
  let mb = fmap (lower . fmap fst . (`getDay` mx)) ma
   in case mx of
        Here x -> There (,) mb mx
        There f x xs -> There (\(e, a) b -> (e, f a b)) (liftA2 (,) mb x) xs

wrapQueue :: Monad f => f (Queue f a) -> Queue f a
wrapQueue f = Day \x -> Phases.lift (fmap (($ x) . getDay ) f)

now :: Applicative f => f a -> Queue f a
now xs = Day \case
  Here x -> There (,) xs (Here x)
  There f y ys -> There (\(a, b) c -> (a, f b c)) (liftA2 (,) xs y) ys

later :: Applicative f => Queue f a -> Queue f a
later q = Day (go q)
  where
    go :: Applicative f => Queue f a -> Phases f b -> Phases f (a, b)
    go (Day f) (Here y) = There (const id) (pure ()) (f (Here y))
    go (Day f) (There g y ys) = There (fmap . g) y (f ys)

delay :: Applicative f => Queue f a -> Queue f a
delay q = Day (go q)
  where
    go (Day f) p@Here {} = There (const id) (pure ()) (f p)
    go (Day f) p@(There g y ys) =
      let mX = liftA2 (,) (f p) ys
       in There (\a ((x, _), b) -> (x, g a b)) y mX
