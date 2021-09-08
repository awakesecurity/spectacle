{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections #-}

-- |
--
-- @since 0.1.0.0
module Language.Spectacle.Checker.Liveness
  ( livenessCheck,
  )
where

import Control.Applicative (Alternative ((<|>)))
import Control.Monad.Reader (MonadReader, ReaderT (runReaderT))
import Control.Monad.State (MonadState, StateT, runStateT)
import Control.Monad.Writer (MonadWriter (tell), WriterT (runWriterT))
import Data.Foldable (maximumBy)
import Data.Function (on, (&))
import Data.IntSet (IntSet)
import qualified Data.IntSet as IntSet
import Data.Kind (Type)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Lens.Micro (Lens', SimpleGetter, lens, to, (^.))
import Lens.Micro.Mtl (use, view, (%=), (.=))

import Control.Monad.Levels (LevelsT, forAp, runLevelsA)
import Language.Spectacle.Checker.Fingerprint (Fingerprint (Fingerprint))
import Language.Spectacle.Checker.LVStateCoverage (LVStateCoverage)
import qualified Language.Spectacle.Checker.LVStateCoverage as LVStateCoverage
import Language.Spectacle.Checker.MCCoverageMap (MCCoverageMap)
import qualified Language.Spectacle.Checker.MCCoverageMap as MCCoverageMap
import Language.Spectacle.Checker.MCError (MCError (MCEventuallyError))
import Language.Spectacle.Checker.MCWorldInfo (MCWorldInfo (MCWorldInfo, mcWorldInfoEnables))
import Language.Spectacle.Specification.Action (ActionInfo (actionInfoFairness), Fairness (Unfair))
import Language.Spectacle.Checker.Model

-- ---------------------------------------------------------------------------------------------------------------------

newtype Liveness :: Type -> Type where
  Liveness ::
    { runLiveness ::
        WriterT [Fingerprint] (LevelsT (ReaderT LVEnv (StateT LVState []))) a
    } ->
    Liveness a
  deriving
    ( Functor
    , Applicative
    , Monad
    , Alternative
    )
  deriving
    ( MonadWriter [Fingerprint]
    , MonadReader LVEnv
    , MonadState LVState
    )

-- ---------------------------------------------------------------------------------------------------------------------

data LVEnv = LVEnv
  { _lvEnvFutureActions :: Set String
  , _lvEnvCoverageMap :: MCCoverageMap
  , _lvEnvActionInfo :: Map String ActionInfo
  }

lvEnvFutureActions :: SimpleGetter LVEnv (Set String)
lvEnvFutureActions = to _lvEnvFutureActions
{-# INLINE lvEnvFutureActions #-}

lvEnvCoverageMap :: SimpleGetter LVEnv MCCoverageMap
lvEnvCoverageMap = to _lvEnvCoverageMap
{-# INLINE lvEnvCoverageMap #-}

lvEnvActionInfo :: Lens' LVEnv (Map String ActionInfo)
lvEnvActionInfo = lens _lvEnvActionInfo \LVEnv {..} x -> LVEnv {_lvEnvActionInfo = x, ..}
{-# INLINE lvEnvActionInfo #-}

-- ---------------------------------------------------------------------------------------------------------------------

data LVState = LVState
  { _lvStateExplored :: LVStateCoverage
  , _lvStateDepth :: Int
  }
  deriving (Eq, Ord, Show)

-- | @since 0.1.0.0
instance Semigroup LVState where
  LVState xs d1 <> LVState ys d2 = LVState (xs `LVStateCoverage.union` ys) (d1 `max` d2)
  {-# INLINE (<>) #-}

-- | @since 0.1.0.0
instance Monoid LVState where
  mempty = LVState LVStateCoverage.empty 0
  {-# INLINE CONLIKE mempty #-}

lvStateExplored :: Lens' LVState LVStateCoverage
lvStateExplored =
  lens
    _lvStateExplored
    \LVState {..} x -> LVState {_lvStateExplored = LVStateCoverage.union x _lvStateExplored, ..}
{-# INLINE lvStateExplored #-}

lvStateDepth :: Lens' LVState Int
lvStateDepth = lens _lvStateDepth \LVState {..} x -> LVState {_lvStateDepth = max x _lvStateDepth, ..}
{-# INLINE lvStateDepth #-}

-- ---------------------------------------------------------------------------------------------------------------------

livenessCheck ::
  Set String ->
  Set Fingerprint ->
  MCCoverageMap ->
  Map String ActionInfo ->
  Either [MCError ctxt] (Set Fingerprint)
livenessCheck unsatisfied fingerprints coverageMap actionInfo = do
  let result =
        stepLivenessInitial fingerprints
          & runLiveness
          & runWriterT
          & runLevelsA
          & flip runReaderT (LVEnv unsatisfied coverageMap actionInfo)
          & flip runStateT (LVState LVStateCoverage.empty 0)
          & concatMap (\(xs, st) -> foldMap (\(unsats, trace) -> [(st, unsats, trace)]) xs)

  if Set.null (foldr (\(_, unsat, _) -> Set.intersection unsat) unsatisfied result)
    then return fingerprints
    else do
      let counterexamples = filter (\(_, unsat', _) -> not (Set.null unsat')) result
          (initialWorld, (lvst, unsat, trace)) =
            counterexamples
              & zip (Set.toList fingerprints)
              & maximumBy (compare `on` (length . \(_, (_, _, xs)) -> xs))

      Left [MCEventuallyError initialWorld (last trace) unsat (lvst ^. lvStateDepth)]

stepLivenessInitial :: Set Fingerprint -> Liveness (Set String)
stepLivenessInitial fingerprints = do
  futureActions <- view lvEnvFutureActions
  satisfied <- traverse (stepLiveness 0 futureActions) (Set.toList fingerprints)

  return (foldr Set.intersection futureActions satisfied)

stepLiveness ::
  Int ->
  Set String ->
  Fingerprint ->
  Liveness (Set String)
stepLiveness depth unsatisfied fpHere
  | Set.null unsatisfied = do
    lvStateExplored %= LVStateCoverage.insert fpHere
    tell [fpHere]

    return unsatisfied
  | otherwise = do
    futureActions <- view lvEnvFutureActions
    nexts <- stepLivenessNext depth fpHere

    lvStateExplored %= LVStateCoverage.insert fpHere
    tell [fpHere]

    checksThere <- forAp nexts \(action, fpsThere) -> do
      let unsatisfied' =
            if action `Set.member` futureActions
              then action `Set.delete` unsatisfied
              else unsatisfied

      checks <- forAp fpsThere \fpThere -> do
        explored <- LVStateCoverage.member fpThere <$> use lvStateExplored

        if explored || fpHere == fpThere
          then do
            lvStateDepth .= depth
            return unsatisfied'
          else do
            return unsatisfied'
              <|> stepLiveness (depth + 1) unsatisfied' fpThere

      return (Set.singleton (action, checks))

    foldr (\(k, v) xs -> xs >>= scheduleLiveness k v) (pure unsatisfied) checksThere

scheduleLiveness :: String -> Set String -> Set String -> Liveness (Set String)
scheduleLiveness action unsatisfied xs = do
  fairness <- maybe Unfair actionInfoFairness . Map.lookup action <$> view lvEnvActionInfo

  if fairness == Unfair
    then return (Set.union unsatisfied xs)
    else return (Set.intersection unsatisfied xs)

stepLivenessNext :: Int -> Fingerprint -> Liveness (Set (String, Set Fingerprint))
stepLivenessNext depth fingerprint = do
  worldInfoHere <- MCCoverageMap.lookup fingerprint <$> view lvEnvCoverageMap

  case worldInfoHere of
    Nothing -> error "no enabled actions"
    Just MCWorldInfo {..} -> do
      let transitions = flip Map.foldMapWithKey mcWorldInfoEnables \k v ->
            Set.singleton (toCanonicalTranition k v)
      fairSteps <- existsScheduledFairAction fingerprint transitions

      if Set.null fairSteps
        then return transitions
        else return fairSteps

existsScheduledFairAction ::
  Fingerprint ->
  Set (String, Set Fingerprint) ->
  Liveness (Set (String, Set Fingerprint))
existsScheduledFairAction worldHere transition = do
  explored <- LVStateCoverage.member worldHere <$> use lvStateExplored

  if explored
    then return Set.empty
    else do
      actionInfo <- view lvEnvActionInfo
      return (modelFairScheduling actionInfo transition)

toCanonicalTranition :: String -> IntSet -> (String, Set Fingerprint)
toCanonicalTranition action nexts = (action, toFingerprintSet nexts)

toFingerprintSet :: IntSet -> Set Fingerprint
toFingerprintSet = IntSet.fold (Set.insert . Fingerprint) Set.empty
