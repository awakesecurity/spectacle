{-# LANGUAGE GeneralizedNewtypeDeriving #-}

-- |
--
-- @since 0.1.0.0
module Language.Spectacle.Behavior
  ( -- * Behavior Streams
    streamBehaviors,
    forgetWorlds,
    pruneCycles,
  )
where

import Control.Applicative (liftA2)
import Control.Comonad.Cofree (Cofree ((:<)), coiterW)
import Control.Monad (unless)
import Control.Monad.Except (Except, MonadError (throwError))
import Control.Monad.State (MonadState, StateT, evalState, modify)
import Data.Foldable (find)
import Data.Maybe (isJust)
import Data.Set (Set)
import qualified Data.Set as Set
import Lens.Micro (SimpleGetter, to)
import Lens.Micro.Mtl (use, view)

import Control.Monad.Levels (forAp)
import Data.Temporal.RSet (Lift (Lift), RSet, intoInterval, liftEval, liftRenew)
import Data.Temporal.Time (Interval (Interval), timeAfter)
import Data.World (World (World), worldFingerprint)
import Language.Spectacle.Checker.Fingerprint (Fingerprint)
import Language.Spectacle.Checker.MCError (MCError (MCAlwaysError))

-- ---------------------------------------------------------------------------------------------------------------------

-- | Like 'shoots', but uses 'null' 'Set' to end the traversal rather than 'null' @g@.
--
-- @since 0.1.0.0
augment :: (Traversable g, Applicative f) => (Set a -> f (Set a)) -> Cofree g (Set a) -> f (Cofree g (Set a))
augment f (here :< there)
  | Set.null here = pure (here :< there)
  | otherwise = liftA2 (:<) (f here) (traverse (augment f) there)
{-# INLINE augment #-}

-- | Stream a sets of intervals reachable by the 'RSet' from the initial set of worlds. The resulting stream can
-- alternatively be viewed as a rose-tree in which:
--
-- * For any interval-node in the tree @Interval t t'@, the parent node is @Interval t' t''@ and the child node is
--   @Interval to t@ for any worlds to, t'.
--
-- * The spanning-tree obtained by traversing a state-graph of the specification that the given 'RSet' implements in a
--   breadth-first way.
--
-- * Each subsequence element of the cofree is a level of the rose-tree.
--
-- @since 0.1.0.0
streamBehaviors ::
  [String] ->
  RSet (World ctxt) (Set (String, Set (World ctxt))) ->
  Set (World ctxt) ->
  Cofree (Except [MCError ctxt]) (Set (Interval (World ctxt) (String, World ctxt)))
streamBehaviors names actions initials = coiterW (behaviorCoalgebra names) (behaviorInitial actions initials)
{-# INLINE streamBehaviors #-}

-- | Constructs an initial comonad needed to coiterate a behavior stream.
--
-- @since 0.1.0.0
behaviorInitial ::
  RSet (World ctxt) (Set (String, Set (World ctxt))) ->
  Set (World ctxt) ->
  Lift RSet (Set (World ctxt)) (Set (Interval (World ctxt) (String, World ctxt)))
behaviorInitial action = Lift (intoInterval action)
{-# INLINE behaviorInitial #-}

-- | The coalgebra generating each successive level of a behavior stream.
--
-- @since 0.1.0.0
behaviorCoalgebra ::
  MonadError [MCError ctxt] m =>
  [String] ->
  Lift RSet (Set (World ctxt)) (Set (Interval (World ctxt) (String, World ctxt))) ->
  m (Lift RSet (Set (World ctxt)) (Set (Interval (World ctxt) (String, World ctxt))))
behaviorCoalgebra names obj = do
  intervals <- reachability names (liftEval obj)
  let nextWorlds = Set.map (snd . timeAfter) intervals
  pure (liftRenew obj nextWorlds)

forgetWorlds ::
  Set (Interval (World ctxt) (String, World ctxt)) ->
  Set (Interval Fingerprint (String, Fingerprint))
forgetWorlds = Set.map \(Interval t (ix, t')) ->
  Interval (view worldFingerprint t) (ix, view worldFingerprint t')

-- | Reachability analysis on globally quantified actions of the given names.
--
-- @since 0.1.0.0
reachability ::
  MonadError [MCError ctxt] m =>
  [String] ->
  Set (Interval (World ctxt) (String, World ctxt)) ->
  m (Set (Interval (World ctxt) (String, World ctxt)))
reachability globals intervals = do
  forAp globals \global -> do
    let reached = find ((== global) . fst . timeAfter) intervals
    unless (isJust reached) do
      throwError [MCAlwaysError undefined (Set.singleton global)]
  return intervals

-- | Prunes subbehaviors extending from worlds which were previously traversed and recorded in ancestor nodes. This has
-- the affect of removing cyclic behaviors from the search tree.
--
-- @since 0.1.0.0
pruneCycles ::
  Traversable m =>
  Cofree m (Set (Interval (World ctxt) (String, World ctxt))) ->
  Cofree m (Set (Interval (World ctxt) (String, World ctxt)))
pruneCycles stream = evalState (augment pruning stream) Set.empty
  where
    pruning ::
      Monad m =>
      Set (Interval (World ctxt) (String, World ctxt)) ->
      StateT (Set Fingerprint) m (Set (Interval (World ctxt) (String, World ctxt)))
    pruning intervals = do
      unseen <- pruneBehaviors intervals
      setMarks (Set.map (snd . timeAfter) unseen)
      return unseen
{-# INLINE pruneCycles #-}

-- | Prunes the set of intervals which have already been traversed.
--
-- @since 0.1.0.0
pruneBehaviors ::
  Monad m =>
  Set (Interval (World ctxt) (String, World ctxt)) ->
  StateT (Set Fingerprint) m (Set (Interval (World ctxt) (String, World ctxt)))
pruneBehaviors intervals
  | Set.null intervals = return Set.empty
  | otherwise = forAp intervals \case
    Interval t (ix, t') -> do
      prune <- use (isMarked t')
      if prune
        then return Set.empty
        else return (Set.singleton (Interval t (ix, t')))

-- | Lens into a 'Set' 'Fingerprint' focusing if the indexed 'World' inhabits the set.
--
-- @since 0.1.0.0
isMarked :: World ctxt -> SimpleGetter (Set Fingerprint) Bool
isMarked (World fp _) = to (Set.member fp)
{-# INLINE isMarked #-}

-- | Marks the set of given worlds as explored.
--
-- @since 0.1.0.0
setMarks :: MonadState (Set Fingerprint) m => Set (World ctxt) -> m ()
setMarks xs = modify (Set.union (Set.map (view worldFingerprint) xs))
{-# INLINE setMarks #-}
