{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiWayIf #-}

module Language.Spectacle.Spec.Prop
  ( -- * Property Checker Monad
    Prop (Prop, unProp),
    runProp,
    checkInvariance,

    -- ** Property Checker State
    PropState (PropState, _infoHere, _infoThere),

    -- *** Lenses
    infoHere,
    infoThere,

    -- ** Property Checker Contexts
    PropCtx (PropCtx, _coverageMap, _worldHere, _worldThere, _behaviorTrace, _disjunctionPath),

    -- *** Lenses
    worldHere,
    worldThere,
    behaviorTrace,
    disjunctionPath,
  )
where

import Control.Monad.Except (ExceptT, MonadError (throwError), runExceptT)
import Control.Monad.Reader
  ( MonadReader (local),
    Reader,
    runReader,
    unless,
  )
import Control.Monad.State (MonadState, StateT (runStateT))
import Lens.Micro
  ( Lens',
    SimpleGetter,
    lens,
    set,
    to,
    (%~),
    (&),
    (.~),
    (^.),
  )
import Lens.Micro.Mtl (use, view, (.=))

import Data.Type.Rec (Rec)
import Language.Spectacle.Exception.ModelCheckerException
  ( CyclicCheckException (CyclicNoInfo, CyclicViolation),
    FormulaException (CyclicException, PropertyViolated),
  )
import Language.Spectacle.Spec.Base (Specifiable)
import Language.Spectacle.Spec.Behavior (Behavior, cyclicSuffixOf, modelsEventually)
import Language.Spectacle.Spec.CheckResult
  ( Additive (unitZero, (>+<)),
    CheckResult (CheckResult),
    Multiplicative (unitOne, (>*<)),
    isComplete,
    isSatisfied,
  )
import Language.Spectacle.Spec.Coverage
  ( CoverageInfo,
    CoverageMap,
    HasCoverageMap (coverageMap),
    checksCompleted,
    subformula,
  )
import Language.Spectacle.Spec.Implication (Modality (ModalAlways, ModalEventually, ModalUpUntil))
import Language.Spectacle.Spec.Zipper (DisjunctZipper (LeftBranch, RightBranch), Junctions (Junctions))
import Language.Spectacle.Syntax.Modal
  ( LTerm
      ( AlwaysL3,
        ComplementL2,
        ConjunctL2,
        ConjunctL4,
        DisjunctL2,
        DisjunctL4,
        EmbedL1,
        EventuallyL3,
        ImpliesL2,
        InfinitelyOftenL3,
        ModalL4,
        NotImpliesL2,
        StaysAsL3,
        UpUntilL3,
        ValueL1
      ),
    Level (L2, L3, L4),
  )

-- ---------------------------------------------------------------------------------------------------------------------

newtype Prop ctx a = Prop
  { unProp ::
      ExceptT FormulaException (StateT (PropState ctx) (Reader (PropCtx ctx))) a
  }
  deriving (Functor, Applicative, Monad)
  deriving
    ( MonadError FormulaException
    , MonadState (PropState ctx)
    , MonadReader (PropCtx ctx)
    )

runProp :: PropState ctx -> PropCtx ctx -> Prop ctx a -> (Either FormulaException a, PropState ctx)
runProp propState propCtx (Prop propM) =
  propM
    & runExceptT
    & flip runStateT propState
    & flip runReader propCtx
{-# INLINE runProp #-}

data PropState ctx = PropState
  { _infoHere :: CoverageInfo ctx
  , _infoThere :: CoverageInfo ctx
  }

infoHere :: Lens' (PropState ctx) (CoverageInfo ctx)
infoHere = lens _infoHere \st info -> st {_infoHere = info}
{-# INLINE infoHere #-}

infoThere :: Lens' (PropState ctx) (CoverageInfo ctx)
infoThere = lens _infoThere \st info -> st {_infoThere = info}
{-# INLINE infoThere #-}

data PropCtx ctx = PropCtx
  { -- | Coverage information accumulated thus far.
    --
    -- @since 0.1.0.0
    _coverageMap :: CoverageMap ctx
  , -- | The world being inspected by the model checker.
    --
    -- @since 0.1.0.0
    _worldHere :: Rec ctx
  , -- | The world one frame ahead of '_worldHere'
    --
    -- @since 0.1.0.0
    _worldThere :: Rec ctx
  , -- | The behavior traced out by the model checker.
    --
    -- @since 0.1.0.0
    _behaviorTrace :: Behavior ctx
  , -- | The path down disjunctions of modal operators this check is committed to.
    --
    -- @since 0.1.0.0
    _disjunctionPath :: Junctions
  }

-- | @since 0.1.0.0
instance HasCoverageMap PropCtx where
  coverageMap = lens _coverageMap \st coverage -> st {_coverageMap = coverage}
  {-# INLINE coverageMap #-}

worldHere :: SimpleGetter (PropCtx ctx) (Rec ctx)
worldHere = to _worldHere
{-# INLINE CONLIKE worldHere #-}

worldThere :: SimpleGetter (PropCtx ctx) (Rec ctx)
worldThere = to _worldHere
{-# INLINE CONLIKE worldThere #-}

behaviorTrace :: SimpleGetter (PropCtx ctx) (Behavior ctx)
behaviorTrace = to _behaviorTrace
{-# INLINE CONLIKE behaviorTrace #-}

disjunctionPath :: Lens' (PropCtx ctx) Junctions
disjunctionPath = lens _disjunctionPath \ctx js -> ctx {_disjunctionPath = js}
{-# INLINE disjunctionPath #-}

-- ---------------------------------------------------------------------------------------------------------------------

checkInvariance :: Specifiable ctx => LTerm 'L4 Bool -> Prop ctx CheckResult
checkInvariance formula = do
  cyclicSuffix <-
    cyclicSuffixOf
      <$> view worldHere
      <*> view worldThere
      <*> view behaviorTrace
  result <- case cyclicSuffix of
    Nothing -> checkFormula formula
    Just suffix -> checkFormulaCyclic suffix formula
  infoHere . checksCompleted .= result ^. isComplete
  return result

-- ---------------------------------------------------------------------------------------------------------------------

checkFormula :: Specifiable ctx => LTerm 'L4 Bool -> Prop ctx CheckResult
checkFormula = \case
  ModalL4 modality -> checkModal modality
  ConjunctL4 left right -> do
    leftResult <- checkFormula left
    rightResult <- checkFormula right
    return (CheckResult (leftResult ^. isSatisfied && rightResult ^. isSatisfied) True unitZero)
  DisjunctL4 left right ->
    view disjunctionPath >>= \case
      Junctions (LeftBranch : js) ->
        local (set disjunctionPath (Junctions js)) (checkFormula left)
      Junctions (RightBranch : js) ->
        local (set disjunctionPath (Junctions js)) (checkFormula right)
      Junctions [] -> undefined -- TODO should be impossible but a made up exception here for when a disjunction is
      -- encountered but the junction stack is empty

checkModal :: Specifiable ctx => LTerm 'L3 Bool -> Prop ctx CheckResult
checkModal = \case
  AlwaysL3 name formula -> checkAlways name formula
  EventuallyL3 name formula -> checkEventually name formula
  UpUntilL3 name leftFormula rightFormula -> checkUpUntil name leftFormula rightFormula
  InfinitelyOftenL3 name formula -> do
    result <- checkExpression formula
    infoHere . subformula name .= result ^. isSatisfied
    return result
  StaysAsL3 name formula -> do
    result <- checkExpression formula
    infoHere . subformula name .= result ^. isSatisfied
    return result

-- | Check if an (assumed to be 'always') qualified formula is satisfied for the previously unseen world currently being
-- inspected by the model checker.
--
-- @since 0.1.0.0
checkAlways :: Specifiable ctx => Int -> LTerm 'L2 Bool -> Prop ctx CheckResult
checkAlways name formula = do
  -- Checking always is simple: we evaluate the formula qualified by always and update the coverage information for the
  -- current world.
  result <- checkExpression formula
  infoHere . subformula name .= result ^. isSatisfied
  if result ^. isSatisfied
    then return result
    else throwPropertyViolation name ModalAlways

-- | Check if an (assumed to be 'eventually') qualified formula is satisfied for the previously unseen world currently
-- being inspected by the model checker.
--
-- @since 0.1.0.0
checkEventually :: Specifiable ctx => Int -> LTerm 'L2 Bool -> Prop ctx CheckResult
checkEventually name formula = do
  -- Because the model checker explores the traces breadth-first, we cannot determine if a formula qualified by
  -- 'eventually' which is not satisfied by the current world will always be false. Doing so would require the model
  -- checker to extend the traces and inspect their worlds a depth-first way. A depth-first search strategy doesn't
  -- work for temporal formula such as
  --
  -- @eventually (p \/ q) == eventually p \/ eventually q@
  --
  -- Since we have no gaurentee that either p or q will be eventually be satisfied before the trace terminates (or enters
  -- a cycle). The way we check eventually works around this challenge by:
  --
  -- 1. Checking if for some world w, @'eventually' p@ is true. If it is false, we say that @'eventually' p@ is assumed
  -- to be true in order to let the model checker continue exploring the model traces. The coverage information for
  -- whether @'eventually' p@ is updated to be the actual result of evaluating p for w and is referred back to in
  -- cycles.
  --
  -- 2. If the trace encounters some world w such that the model's termination condition is met, we must show that some
  -- world in the trace satisfies the eventually of p, otherwise we know that we can safely emit an exception that the
  -- invariant eventually of p was violated.
  --
  -- 3. If the trace enters into a cycle, we know that some world within the cycle must satisfy eventually of p.
  -- Otherwise, we know it is false since all extensions of the trade imply @'eventually' p == False@
  result <- checkExpression formula
  if result ^. isSatisfied
    then do
      infoHere . subformula name .= True
      return result
    else do
      infoHere . subformula name .= False
      return (result & isComplete .~ False)

-- | Check that the formulas @p@, @q@ for some @'upUntil' p q@ are satisfied for the previously unseen world currently
-- being inspected by the model checker.
--
-- @since 0.1.0.0
checkUpUntil :: Specifiable ctx => Int -> LTerm 'L2 Bool -> LTerm 'L2 Bool -> Prop ctx CheckResult
checkUpUntil name leftFormula rightFormula = do
  -- 'upUntil' is checked just as 'eventually' is, except for in addition to these checks we must show that for some
  -- @'upUntil' p q@, p is true when q is not.
  leftResult <- checkExpression leftFormula
  rightResult <- checkExpression rightFormula
  if rightResult ^. isSatisfied
    then do
      infoHere . subformula name .= True
      return (rightResult & isComplete .~ True)
    else do
      unless (leftResult ^. isSatisfied) (throwPropertyViolation name ModalUpUntil)
      infoHere . subformula name .= False
      return rightResult

checkExpression :: Specifiable ctx => LTerm 'L2 Bool -> Prop ctx CheckResult
checkExpression = \case
  EmbedL1 (ValueL1 x) -> return (CheckResult x False unitZero)
  ConjunctL2 lefts rights -> do
    leftResult <- checkExpression lefts
    rightResult <- checkExpression rights
    return (leftResult >*< rightResult)
  DisjunctL2 lefts rights -> do
    leftResult <- checkExpression lefts
    rightResult <- checkExpression rights
    if
        | leftResult ^. isSatisfied && rightResult ^. isSatisfied -> return (leftResult >+< rightResult)
        | leftResult ^. isSatisfied -> return leftResult
        | rightResult ^. isSatisfied -> return rightResult
        | otherwise -> return unitZero
  ComplementL2 terms -> do
    result <- checkExpression terms
    return (result & isSatisfied %~ not)
  ImpliesL2 lefts rights -> do
    leftResult <- checkExpression lefts
    rightResult <- checkModal rights
    if
        | leftResult ^. isSatisfied && rightResult ^. isSatisfied -> return (leftResult >*< rightResult)
        | not (leftResult ^. isSatisfied) && rightResult ^. isSatisfied -> return rightResult
        | otherwise -> return unitZero
  NotImpliesL2 lefts rights -> do
    leftResult <- checkExpression lefts
    rightResult <- checkModal rights
    if leftResult ^. isSatisfied && not (rightResult ^. isSatisfied)
      then return (leftResult >*< rightResult)
      else return unitZero

-- ---------------------------------------------------------------------------------------------------------------------

checkFormulaCyclic :: Specifiable ctx => Behavior ctx -> LTerm 'L4 Bool -> Prop ctx CheckResult
checkFormulaCyclic cyclicBehavior = \case
  ModalL4 modality -> checkModalCyclic cyclicBehavior modality
  ConjunctL4 left right -> do
    leftResult <- checkFormulaCyclic cyclicBehavior left
    rightResult <- checkFormulaCyclic cyclicBehavior right
    return (CheckResult (leftResult ^. isSatisfied && rightResult ^. isSatisfied) True unitZero)
  DisjunctL4 left right ->
    view disjunctionPath >>= \case
      Junctions (LeftBranch : js) ->
        local (set disjunctionPath (Junctions js)) (checkFormulaCyclic cyclicBehavior left)
      Junctions (RightBranch : js) ->
        local (set disjunctionPath (Junctions js)) (checkFormulaCyclic cyclicBehavior right)
      Junctions [] -> undefined -- TODO should be impossible but a made up exception here for when a disjunction is
      -- encountered but the junction stack is empty

checkModalCyclic :: Specifiable ctx => Behavior ctx -> LTerm 'L3 Bool -> Prop ctx CheckResult
checkModalCyclic cyclicBehavior = \case
  AlwaysL3 name _ -> checkAlwaysCyclic name
  EventuallyL3 name _ -> checkEventuallyCyclic name cyclicBehavior
  UpUntilL3 name leftFormula _ -> checkUpUntilCyclic name cyclicBehavior leftFormula
  InfinitelyOftenL3 name formula -> do
    sat <- checkExpressionCyclic formula
    infoHere . subformula name .= sat
    return (CheckResult sat True unitZero)
  StaysAsL3 name formula -> do
    sat <- checkExpressionCyclic formula
    infoHere . subformula name .= sat
    return (CheckResult sat True unitZero)

-- | Check whether an (assumed to be 'always') qualified formula is satisfied within the context of a cyclic trace
-- for the world currently being inspected by the model checker.
--
-- @since 0.1.0.0
checkAlwaysCyclic :: Specifiable ctx => Int -> Prop ctx CheckResult
checkAlwaysCyclic name = do
  satHere <- use (infoHere . subformula name)
  satThere <- use (infoThere . subformula name)
  unless (satHere && satThere) (throwCyclicViolation name ModalAlways)
  return (unitOne & isComplete .~ True)

-- | Check whether an (assumed to be 'eventually') qualified formula is satisfied within the context of a cyclic trace
-- for the world currently being inspected by the model checker.
--
-- @since 0.1.0.0
checkEventuallyCyclic :: Specifiable ctx => Int -> Behavior ctx -> Prop ctx CheckResult
checkEventuallyCyclic name cyclicBehavior = do
  satHere <- use (infoHere . subformula name)
  if satHere
    then return (unitOne & isComplete .~ True)
    else do
      doesModel <-
        modelsEventually name
          <$> view coverageMap
          <*> pure cyclicBehavior
      unless doesModel (throwCyclicNoInfo name ModalEventually)
      return (unitOne & isComplete .~ True)

-- | Check whether an (assumed to be 'upUntil') qualified formula is satisfied within the context of a cyclic trace
-- for the world currently being inspected by the model checker.
--
-- @since 0.1.0.0
checkUpUntilCyclic :: Specifiable ctx => Int -> Behavior ctx -> LTerm 'L2 Bool -> Prop ctx CheckResult
checkUpUntilCyclic name cyclicBehavior formula = do
  satHere <- use (infoHere . subformula name)
  if satHere
    then return (unitOne & isComplete .~ True)
    else do
      leftSat <- checkExpressionCyclic formula
      doesModel <-
        modelsEventually name
          <$> view coverageMap
          <*> pure cyclicBehavior
      unless (doesModel && leftSat) (throwCyclicNoInfo name ModalUpUntil)
      return (unitOne & isComplete .~ True)

checkExpressionCyclic :: forall ctx. Specifiable ctx => LTerm 'L2 Bool -> Prop ctx Bool
checkExpressionCyclic = \case
  EmbedL1 (ValueL1 x) -> return x
  ConjunctL2 lefts rights -> do
    leftResult <- checkExpressionCyclic lefts
    rightResult <- checkExpressionCyclic rights
    return (leftResult && rightResult)
  DisjunctL2 lefts rights -> do
    leftResult <- checkExpressionCyclic lefts
    rightResult <- checkExpressionCyclic rights
    return (leftResult || rightResult)
  ComplementL2 terms -> not <$> checkExpressionCyclic terms
  _ -> return True

-- ---------------------------------------------------------------------------------------------------------------------

throwCyclicNoInfo :: Show (Rec ctx) => Int -> Modality -> Prop ctx a
throwCyclicNoInfo name modality = do
  here <- view worldHere
  throwError (CyclicException (CyclicNoInfo name modality here))

throwPropertyViolation :: Show (Rec ctx) => Int -> Modality -> Prop ctx a
throwPropertyViolation name modality = do
  here <- view worldHere
  throwError (PropertyViolated name modality here)
{-# INLINE throwPropertyViolation #-}

throwCyclicViolation :: Show (Rec ctx) => Int -> Modality -> Prop ctx a
throwCyclicViolation name modality = do
  here <- view worldHere
  there <- view worldThere
  throwError (CyclicException (CyclicViolation name modality here there))
{-# INLINE throwCyclicViolation #-}
