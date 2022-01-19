{-# LANGUAGE TupleSections #-}

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
