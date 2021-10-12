-- | Effect queues.
--
-- @since 0.1.0.0
module Control.Applicative.Queue
  ( Queue,
    runQueue,
    wrap,
    joinQueue,
    tensor,
    now,
    later,
  )
where

import Control.Applicative (Applicative (liftA2))
import Control.Monad.Zip (MonadZip)

import Control.Applicative.Day (Day (Day), getDay)
import Control.Applicative.Phases (Phases (Here, There), lower)
import qualified Control.Applicative.Phases as Phases

-- ---------------------------------------------------------------------------------------------------------------------

type Queue f = Day (Phases f)

runQueue :: Applicative f => Queue f a -> f a
runQueue = fmap fst . Phases.lower . flip getDay (Here ())

wrap :: Monad f => f (Queue f a) -> Queue f a
wrap f = Day \x -> Phases.lift (fmap (($ x) . getDay) f)

joinQueue :: Monad f => Queue f (Queue f a) -> Queue f a
joinQueue (Day f) = Day \x ->
  let x' = lower (fmap fst (f x))
   in getDay (wrap x') x

-- | Tensoring effect queues. Left-biased on the quantified 'Day' variable so beware.
--
-- @since 0.1.0.0
tensor :: (Applicative f, MonadZip g, Monoid a) => Queue f (g a) -> Queue f (g a) -> Queue f (g a)
tensor (Day f) (Day g) = Day \x ->
  let here = Phases.tensor (fmap fst (f x)) (fmap fst (g x))
      there = fmap snd (f x)
   in liftA2 (,) here there

now :: Applicative f => f a -> Queue f a
now xs = Day \case
  Here x -> There (,) xs (Here x)
  There f y ys -> There (\(x, y) z -> (x, f y z)) (liftA2 (,) xs y) ys

later :: Applicative f => Queue f a -> Queue f a
later xs = Day (delay xs)
  where
    delay :: Applicative f => Queue f a -> Phases f b -> Phases f (a, b)
    delay (Day f) (Here y) = There (const id) (pure ()) (f (Here y))
    delay (Day f) (There g y ys) = There (fmap . g) y (f ys)
