-- | Effect queues.
--
-- @since 0.1.0.0
module Control.Applicative.Queue
  ( Queue,
    runQueue,
    liftQueue,
    wrapQueue,
    joinQueue,
    now,
    later,
  )
where

import Control.Applicative (Applicative (liftA2))

import Control.Applicative.Day (Day (Day), getDay)
import Control.Applicative.Phases (Phases (Here, There), liftPhases, lowerPhases, wrapPhases)

-- ---------------------------------------------------------------------------------------------------------------------

type Queue f = Day (Phases f)

runQueue :: Applicative f => Queue f a -> f a
runQueue = fmap fst . lowerPhases . flip getDay (Here ())

liftQueue :: Monad f => Queue f (f a) -> Queue f a
liftQueue (Day f) = Day \x ->
  let fx = fmap fst (f x)
      fy = fmap snd (f (fmap snd (f x)))
   in liftA2 (,) (liftPhases fx) fy

wrapQueue :: Monad f => f (Queue f a) -> Queue f a
wrapQueue f = Day \x -> wrapPhases (fmap (($ x) . getDay) f)

joinQueue :: Monad f => Queue f (Queue f a) -> Queue f a
joinQueue (Day f) = Day \x ->
  let x' = lowerPhases (fmap fst (f x))
   in getDay (wrapQueue x') x

now :: Applicative f => f a -> Queue f a
now xs = Day \case
  Here x -> There (,) xs (Here x)
  There f y ys -> There (\(x, y) z -> (x, f y z)) (liftA2 (,) xs y) ys

later :: Applicative f => Queue f a -> Queue f a
later xs = Day \case
  Here x -> There (const id) (pure ()) (getDay xs (Here x))
  There f y ys -> There (\x (y, z) -> (y, f x z)) y (getDay xs ys)
