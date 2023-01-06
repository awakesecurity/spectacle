-- |
-- Module      :  Control.Applicative.Queue
-- Copyright   :  (c) Arista Networks, 2022-2023
-- License     :  Apache License 2.0, see LICENSE
--
-- Stability   :  stable
-- Portability :  non-portable (GHC extensions)
--
-- Effect queues.
--
-- @since 1.0.0
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

-- | A queue of (homogeneous) effects of type @f@.
--
-- @since 1.0.0
type Queue f = Day (Phases f)

-- | Execute a @'Queue'@ and yield the result.
runQueue :: Applicative f => Queue f a -> f a
runQueue q = fmap fst (lowerPhases (getDay q (Here ())))

-- | Lift a queue containing effects @f a@ in @f@ into a queue of values @a@ in @f@.
liftQueue :: Monad f => Queue f (f a) -> Queue f a
liftQueue (Day f) = Day \x ->
  let fx = fmap fst (f x)
      fy = fmap snd (f (fmap snd (f x)))
   in liftA2 (,) (liftPhases fx) fy

-- | Wrap an effect over @f@ that produces a @'Queue'@ into a @'Queue'@ itself.
wrapQueue :: Monad f => f (Queue f a) -> Queue f a
wrapQueue f = Day \x ->
  let k a b = getDay b a -- NB: *necessary* for simplified subsumption in GHC 9.0, sigh...
   in wrapPhases (fmap (k x) f)

-- | Join an effectful @'Queue'@ of effectful @'Queue'@s into a single @'Queue'@.
joinQueue :: Monad f => Queue f (Queue f a) -> Queue f a
joinQueue (Day f) = Day \x ->
  let x' = lowerPhases (fmap fst (f x))
   in getDay (wrapQueue x') x

-- | Convert any applicative @f@ into a @'Queue'@.
now :: Applicative f => f a -> Queue f a
now xs = Day \case
  Here x -> There (,) xs (Here x)
  There f y ys -> There (\(a, b) c -> (a, f b c)) (liftA2 (,) xs y) ys

-- | Given a @'Queue'@ of effects, produce a new @'Queue'@ that produces its next effect later.
later :: Applicative f => Queue f a -> Queue f a
later q = Day (go q)
  where
    go :: Applicative f => Queue f a -> Phases f b -> Phases f (a, b)
    go (Day f) (Here y) = There (const id) (pure ()) (f (Here y))
    go (Day f) (There g y ys) = There (fmap . g) y (f ys)
