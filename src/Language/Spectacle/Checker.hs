{-# LANGUAGE GeneralisedNewtypeDeriving #-}
{-# LANGUAGE UndecidableInstances #-}

module Language.Spectacle.Checker
  ( CheckerState (..),
    modelCheck,
  )
where

import Control.Concurrent.Async.Lifted.Safe (Forall, Pure)
import Control.Monad
  ( filterM,
    join,
    unless,
    when,
  )
import Control.Monad.Base (MonadBase)
import Control.Monad.Catch (MonadCatch, MonadThrow)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.State.Strict
  ( MonadState,
    StateT,
    evalStateT,
    get,
    gets,
    modify,
  )
import Control.Monad.Trans.Class (MonadTrans, lift)
import Control.Monad.Trans.Control (MonadBaseControl)
import Data.Foldable (for_)
import Data.Hashable (Hashable, hash)
import Data.IntMap (IntMap)
import qualified Data.IntMap as IntMap
import Data.List (nub)

import Control.Monad.Mapping (MappingT)
import Data.Either (rights)
import Data.Type.HList (HList, HListT)
import Language.Spectacle.Spec (runSpec)

-- -----------------------------------------------------------------------------

-- | Constraints required to do a model check.
--
-- @since 0.1.0.0
type MonadSpec m sig =
  ( MonadBaseControl IO m
  , MonadCatch m
  , Hashable (Vars sig)
  )

-- | The 'Checker' monad transformer.
--
-- @since 0.1.0.0
newtype Checker sig m r = Checker
  {unChecker :: StateT (CheckerState sig) m r}
  deriving
    ( Functor
    , Applicative
    , Monad
    , MonadIO
    , MonadCatch
    , MonadThrow
    , MonadState (CheckerState sig)
    , MonadTrans
    , MonadBase b
    , MonadBaseControl b
    )

-- | The inner state 'Checker' uses.
--
-- @since 0.1.0.0
data CheckerState sig = CheckerState
  { toExplore :: [Vars sig]
  , fingerprints :: Fingerprints
  , invariant :: HList sig -> Bool
  , failures :: [Vars sig]
  , goal :: HList sig -> Bool
  , successes :: [Vars sig]
  }

-- | A collection of variables making up a model's state.
--
-- @since 0.1.0.0
type Vars sig = HList sig

-- | Run a model check.
--
-- @since 0.1.0.0
modelCheck ::
  (Forall (Pure m), MonadSpec m sig, Show (Vars sig), Eq (Vars sig)) =>
  [Relation m sig] ->
  (Vars sig -> Bool) ->
  (Vars sig -> Bool) ->
  Vars sig ->
  m (Either [HList sig] (CheckerState sig))
modelCheck rels invar event initSt =
  let initCkrSt =
        CheckerState
          { toExplore = [initSt]
          , fingerprints = mempty
          , invariant = invar
          , failures = []
          , goal = event
          , successes = []
          }
   in runChecker rels initCkrSt

-- | Run the 'Checker''s inner 'StateT'.
runChecker ::
  (Forall (Pure m), MonadSpec m sig, Show (Vars sig), Eq (Vars sig)) =>
  [Relation m sig] ->
  CheckerState sig ->
  m (Either [HList sig] (CheckerState sig))
runChecker rels = evalStateT (unChecker (modelChecker rels))

-- | Monadic action preforming an exhaustive search of a state space for the
-- next-state relation provided.
--
-- @since 0.1.0.0
modelChecker ::
  (Forall (Pure m), MonadSpec m sig, Show (Vars sig), Eq (Vars sig)) =>
  [Relation m sig] ->
  Checker sig m (Either [HList sig] (CheckerState sig))
modelChecker rels = do
  eventually <- gets goal
  invar <- gets invariant
  -- Flush the queue of states to check and record their state fingerprints.
  toCheck <- flushToExplore
  for_ toCheck newFingerprint
  -- Push any states which satisfy a goal/break an invariant then record those.
  for_ toCheck \var -> do
    unless (invar var) $ pushFailure var
    when (eventually var) $ pushSuccess var
  -- Get the new states and continue.
  fails <- gets failures
  if null fails
    then do
      newStates <- section rels toCheck
      if null newStates
        then do
          wins <- gets successes
          if null wins
            then -- We cannot continue because every relation in the disjunction fails,
            -- nothing satisfying "eventually" has been found so this amounts to
            -- an error.
              return (Left [])
            else -- Every disjunction fails but we have found successful states so our
            -- model check was successful.
              Right <$> get
        else do
          pushToExplore (nub newStates)
          modelChecker rels
    else return (Left fails)

-- -----------------------------------------------------------------------------

-- | Flush the queue of states the model checker needs to explore.
--
-- @since 0.1.0.0
flushToExplore :: MonadSpec m sig => Checker sig m [Vars sig]
flushToExplore = do
  toexplore <- gets toExplore
  modify \checkerState -> checkerState {toExplore = []}
  return toexplore
{-# INLINE flushToExplore #-}

-- | Push new states to explore onto the 'toExplore' queue.
--
-- @since 0.1.0.0
pushToExplore :: MonadSpec m sig => [Vars sig] -> Checker sig m ()
pushToExplore vars = modify \checkerState ->
  checkerState {toExplore = vars}
{-# INLINE pushToExplore #-}

-- | Push a state which satisfies an "eventually" predicate.
--
-- @since 0.1.0.0
pushSuccess :: MonadSpec m sig => Vars sig -> Checker sig m ()
pushSuccess vars = modify \checkerState ->
  checkerState
    { successes = vars : successes checkerState
    }
{-# INLINE pushSuccess #-}

-- | Push a failure which breaks the model checkers invariant.
--
-- @since 0.1.0.0
pushFailure :: MonadSpec m sig => Vars sig -> Checker sig m ()
pushFailure vars = modify \checkerState ->
  checkerState
    { failures = vars : failures checkerState
    }
{-# INLINE pushFailure #-}

-- -----------------------------------------------------------------------------

-- | The type of fingerprints is an 'Data.IntMap.Strict' which only records
-- hashes.
--
-- @since 0.1.0.0
type Fingerprints = IntMap ()

-- | Record the hash of a given state into a model checker's fingerprints.
--
-- @since 0.1.0.0
newFingerprint :: MonadSpec m sig => Vars sig -> Checker sig m ()
newFingerprint vars = do
  prints <- gets fingerprints
  modify \checkerState ->
    checkerState
      { fingerprints = IntMap.insert (hash vars) () prints
      }
{-# INLINE newFingerprint #-}

-- -----------------------------------------------------------------------------

-- | A next-state relation.
--
-- @since 0.1.0.0
type Relation m sig = HListT (MappingT sig m) sig

-- | "section" a next-state relation to the new reachable states by that
-- relation.
--
-- @since 0.1.0.0
section ::
  (Forall (Pure m), MonadSpec m sig) =>
  [Relation m sig] ->
  [Vars sig] ->
  Checker sig m [Vars sig]
section rels vars = join <$> traverse (`branchRelation` vars) rels
{-# INLINE section #-}

-- | Branch a relation within a next-state relation disjunction into its new
-- reachable states.
--
-- @since 0.1.0.0
branchRelation ::
  (Forall (Pure m), MonadSpec m sig) =>
  Relation m sig ->
  [Vars sig] ->
  Checker sig m [Vars sig]
branchRelation rel vars = do
  newSts <- lift (traverse (`runSpec` rel) vars)
  reduceToUnseen (rights newSts)
{-# INLINE branchRelation #-}

-- | Reduce the new states given by a relation into states the checker has not
-- yet explored.
--
-- @since 0.1.0.0
reduceToUnseen :: MonadSpec m sig => [Vars sig] -> Checker sig m [Vars sig]
reduceToUnseen vars = do
  prints <- gets fingerprints
  filterM (return . not . isSeen prints) vars
{-# INLINE reduceToUnseen #-}

-- | Predicate checking if a given state has been explored by the model checker.
--
-- @since 0.1.0.0
isSeen :: Hashable (Vars sig) => Fingerprints -> Vars sig -> Bool
isSeen prints x = IntMap.member (hash x) prints
{-# INLINE isSeen #-}
