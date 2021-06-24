{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiWayIf #-}

module Language.Spectacle.Spec.Prop
  ( interpretFormula,
    checkFormula,
    checkAlways,
    checkEventually,
    checkUpUntil,
    checkFormulaClosed,
    checkAlwaysClosed,
    checkEventuallyClosed,
    checkUpUntilClosed,
  )
where

import Control.Monad.Except (MonadError (throwError), unless)
import Control.Monad.Reader (MonadReader (local))
import Lens.Micro (set, (%~), (&), (.~), (^.))
import Lens.Micro.Mtl (use, view, (.=))

import Data.Type.Rec (Rec)
import Language.Spectacle.Exception.ModelCheckerException
  ( CyclicCheckException (CyclicViolation),
    FormulaException (CyclicException, PropertyViolated),
  )
import Language.Spectacle.Spec.Base (Modality (ModalAlways, ModalEventually, ModalUpUntil), Specifiable)
import Language.Spectacle.Spec.Behavior (Behavior, cyclicSuffixOf, modelsEventually)
import Language.Spectacle.Spec.CheckResult
  ( CheckResult (CheckResult),
    Multiplicative (unitOne, (>*<)),
    isComplete,
    isSatisfied,
  )
import Language.Spectacle.Spec.Coverage (HasCoverageMap (coverageMap), checksCompleted, subformula)
import Language.Spectacle.Spec.Prop.Base
  ( Prop,
    behaviorTrace,
    disjunctionPath,
    infoHere,
    infoThere,
    worldHere,
    worldThere,
  )
import Language.Spectacle.Spec.Zipper
  ( DisjunctZipper (LeftBranch, RightBranch),
    Junctions (Junctions),
  )
import Language.Spectacle.Syntax.Modal.Term
  ( Term (Always, Complement, Conjunct, Disjunct, Eventually, InfinitelyOften, StaysAs, UpUntil, Value),
  )

-- ---------------------------------------------------------------------------------------------------------------------

interpretFormula :: Specifiable ctx => Term Bool -> Prop ctx CheckResult
interpretFormula formula = do
  cyclicSuffix <-
    cyclicSuffixOf
      <$> view worldHere
      <*> view worldThere
      <*> view behaviorTrace
  result <- case cyclicSuffix of
    Nothing -> checkFormula formula
    Just suffix -> checkFormulaClosed suffix formula
  infoHere . checksCompleted .= result ^. isComplete
  return result

checkFormula :: Specifiable ctx => Term Bool -> Prop ctx CheckResult
checkFormula = \case
  Value x -> return (CheckResult x True)
  Conjunct e1 e2 -> do
    result1 <- checkFormula e1
    result2 <- checkFormula e2
    return (result1 >*< result2)
  Disjunct e1 e2 -> do
    branches <- view disjunctionPath
    case branches of
      Junctions (LeftBranch : js) -> do
        local (set disjunctionPath (Junctions js)) (checkFormula e1)
      Junctions (RightBranch : js) -> do
        local (set disjunctionPath (Junctions js)) (checkFormula e2)
      Junctions [] -> undefined -- TODO
  Complement e -> do
    result <- checkFormula e
    return (result & isSatisfied %~ not)
  Always name e -> checkAlways name e
  Eventually name e -> checkEventually name e
  UpUntil name e1 e2 -> checkUpUntil name e1 e2
  StaysAs name e -> do
    result <- checkFormula e
    infoHere . subformula name .= result ^. isSatisfied
    return result
  InfinitelyOften name e -> do
    result <- checkFormula e
    infoHere . subformula name .= result ^. isSatisfied
    return result

checkAlways :: Specifiable ctx => Int -> Term Bool -> Prop ctx CheckResult
checkAlways name e = do
  isSat <- use (infoHere . subformula name)
  if isSat
    then return (unitOne & isComplete .~ True)
    else do
      result <- checkFormula e
      infoHere . subformula name .= result ^. isSatisfied
      if result ^. isSatisfied
        then return result
        else throwPropertyViolation name ModalAlways

checkEventually :: Specifiable ctx => Int -> Term Bool -> Prop ctx CheckResult
checkEventually name e = do
  isSat <- use (infoHere . subformula name)
  if isSat
    then do
      infoHere . subformula name .= True
      infoThere . subformula name .= True
      return (unitOne & isComplete .~ True)
    else do
      result <- checkFormula e
      if result ^. isSatisfied || isSat
        then do
          infoHere . subformula name .= True
          infoThere . subformula name .= True
          return result
        else do
          let result' =
                result
                  & isSatisfied .~ True
                  & isComplete .~ False
          infoHere . subformula name .= False
          return result'

checkUpUntil :: Specifiable ctx => Int -> Term Bool -> Term Bool -> Prop ctx CheckResult
checkUpUntil name e1 e2 = do
  result1 <- checkFormula e1
  result2 <- checkFormula e2
  if result2 ^. isSatisfied
    then do
      infoHere . subformula name .= True
      return result2
    else
      if result1 ^. isSatisfied
        then do
          let result1' =
                result1
                  & isSatisfied .~ True
                  & isComplete .~ False
          infoHere . subformula name .= False
          return result1'
        else throwPropertyViolation name ModalUpUntil

checkFormulaClosed :: Specifiable ctx => Behavior ctx -> Term Bool -> Prop ctx CheckResult
checkFormulaClosed closure = \case
  Value x -> return (CheckResult x True)
  Conjunct e1 e2 -> do
    result1 <- checkFormulaClosed closure e1
    result2 <- checkFormulaClosed closure e2
    return (result1 >*< result2)
  Disjunct e1 e2 -> do
    branches <- view disjunctionPath
    case branches of
      Junctions (LeftBranch : js) -> local (set disjunctionPath (Junctions js)) (checkFormulaClosed closure e1)
      Junctions (RightBranch : js) -> local (set disjunctionPath (Junctions js)) (checkFormulaClosed closure e2)
      Junctions [] -> undefined -- TODO
  Complement e -> do
    result <- checkFormulaClosed closure e
    return (result & isSatisfied %~ not)
  Always name _ -> checkAlwaysClosed name
  Eventually name _ -> checkEventuallyClosed name closure
  UpUntil name e1 _ -> checkUpUntilClosed name e1 closure
  StaysAs name e -> do
    result <- checkFormulaClosed closure e
    infoHere . subformula name .= result ^. isSatisfied
    return result
  InfinitelyOften name e -> do
    result <- checkFormulaClosed closure e
    infoHere . subformula name .= result ^. isSatisfied
    return result

checkAlwaysClosed :: Show (Rec ctx) => Int -> Prop ctx CheckResult
checkAlwaysClosed name = do
  satHere <- use (infoHere . subformula name)
  satThere <- use (infoThere . subformula name)
  unless (satHere && satThere) (throwCyclicViolation name ModalAlways)
  return (unitOne & isComplete .~ True)

checkEventuallyClosed :: Specifiable ctx => Int -> Behavior ctx -> Prop ctx CheckResult
checkEventuallyClosed name closure = do
  satHere <- use (infoHere . subformula name)
  if satHere
    then return (unitOne & isComplete .~ True)
    else do
      sat <-
        modelsEventually name
          <$> view coverageMap
          <*> pure closure
      unless sat (throwCyclicViolation name ModalEventually)
      return (unitOne & isComplete .~ True)

checkUpUntilClosed :: Specifiable ctx => Int -> Term Bool -> Behavior ctx -> Prop ctx CheckResult
checkUpUntilClosed name e closure = do
  satHere <- use (infoHere . subformula name)
  if satHere
    then return (unitOne & isComplete .~ True)
    else do
      result <- checkFormula e
      models <-
        modelsEventually name
          <$> view coverageMap
          <*> pure closure
      unless (result ^. isSatisfied && models) (throwCyclicViolation name ModalUpUntil)
      return (unitOne & isComplete .~ True)

throwPropertyViolation :: Show (Rec ctx) => Int -> Modality -> Prop ctx a
throwPropertyViolation name modality = do
  here <- view worldHere
  throwError (PropertyViolated name modality here)

throwCyclicViolation :: Show (Rec ctx) => Int -> Modality -> Prop ctx a
throwCyclicViolation name modality = do
  here <- view worldHere
  there <- view worldThere
  throwError (CyclicException (CyclicViolation name modality here there))

-- checkInvariance :: Specifiable ctx => Term Bool -> Prop ctx CheckResult
-- checkInvariance formula = do
--   cyclicSuffix <-
--     cyclicSuffixOf
--       <$> view worldHere
--       <*> view worldThere
--       <*> view behaviorTrace
--   result <- case cyclicSuffix of
--     Nothing -> checkFormula formula
--     Just suffix -> checkFormulaCyclic suffix formula
--   infoHere . checksCompleted .= result ^. isComplete
--   return result

-- -- ---------------------------------------------------------------------------------------------------------------------

-- checkFormula :: Specifiable ctx => Term Bool -> Prop ctx CheckResult
-- checkFormula = \case
--   Conjunct left right -> do
--     leftResult <- checkFormula left
--     rightResult <- checkFormula right
--     return (CheckResult (leftResult ^. isSatisfied && rightResult ^. isSatisfied) True unitZero)
--   Disjunct left right ->
--     view disjunctionPath >>= \case
--       Junctions (LeftBranch : js) ->
--         local (set disjunctionPath (Junctions js)) (checkFormula left)
--       Junctions (RightBranch : js) ->
--         local (set disjunctionPath (Junctions js)) (checkFormula right)
--       Junctions [] -> undefined -- TODO should be impossible but a made up exception here for when a disjunction is
--       -- encountered but the junction stack is empty
--   Always name formula -> checkAlways name formula
--   Eventually name formula -> checkEventually name formula
--   UpUntil name leftFormula rightFormula -> checkUpUntil name leftFormula rightFormula
--   InfinitelyOften name formula -> do
--     result <- checkFormula formula
--     infoHere . subformula name .= result ^. isSatisfied
--     return result
--   StaysAs name formula -> do
--     result <- checkFormula formula
--     infoHere . subformula name .= result ^. isSatisfied
--     return result
--   Value x -> return (CheckResult x True unitZero)
--   Conjunct lefts rights -> do
--     leftResult <- checkFormula lefts
--     rightResult <- checkFormula rights
--     return (leftResult >*< rightResult)
--   Disjunct lefts rights -> do
--     leftResult <- checkFormula lefts
--     rightResult <- checkFormula rights
--     if
--         | leftResult ^. isSatisfied && rightResult ^. isSatisfied -> return (leftResult >+< rightResult)
--         | leftResult ^. isSatisfied -> return leftResult
--         | rightResult ^. isSatisfied -> return rightResult
--         | otherwise -> return unitZero
--   Complement terms -> do
--     result <- checkFormula terms
--     return (result & isSatisfied %~ not)
--   Implies lefts rights -> do
--     leftResult <- checkFormula lefts
--     rightResult <- checkFormula rights
--     if
--         | leftResult ^. isSatisfied && rightResult ^. isSatisfied -> return (leftResult >*< rightResult)
--         | not (leftResult ^. isSatisfied) && rightResult ^. isSatisfied -> return rightResult
--         | otherwise -> trace "lala" (return unitOne)
--   NotImplies lefts rights -> do
--     leftResult <- checkFormula lefts
--     rightResult <- checkFormula rights
--     if leftResult ^. isSatisfied && not (rightResult ^. isSatisfied)
--       then return (leftResult >*< rightResult)
--       else return unitZero

-- -- | Check if an (assumed to be 'always') qualified formula is satisfied for the previously unseen world currently being
-- -- inspected by the model checker.
-- --
-- -- @since 0.1.0.0
-- checkAlways :: Specifiable ctx => Int -> Term Bool -> Prop ctx CheckResult
-- checkAlways name formula = do
--   -- Checking always is simple: we evaluate the formula qualified by always and update the coverage information for the
--   -- current world.
--   result <- checkFormula formula
--   infoHere . subformula name .= result ^. isSatisfied
--   if result ^. isSatisfied
--     then return result
--     else throwPropertyViolation name ModalAlways

-- -- | Check if an (assumed to be 'eventually') qualified formula is satisfied for the previously unseen world currently
-- -- being inspected by the model checker.
-- --
-- -- @since 0.1.0.0
-- checkEventually :: Specifiable ctx => Int -> Term Bool -> Prop ctx CheckResult
-- checkEventually name formula = do
--   -- Because the model checker explores the traces breadth-first, we cannot determine if a formula qualified by
--   -- 'eventually' which is not satisfied by the current world will always be false. Doing so would require the model
--   -- checker to extend the traces and inspect their worlds a depth-first way. A depth-first search strategy doesn't
--   -- work for temporal formula such as
--   --
--   -- @eventually (p \/ q) == eventually p \/ eventually q@
--   --
--   -- Since we have no gaurentee that either p or q will be eventually be satisfied before the trace terminates (or enters
--   -- a cycle). The way we check eventually works around this challenge by:
--   --
--   -- 1. Checking if for some world w, @'eventually' p@ is true. If it is false, we say that @'eventually' p@ is assumed
--   -- to be true in order to let the model checker continue exploring the model traces. The coverage information for
--   -- whether @'eventually' p@ is updated to be the actual result of evaluating p for w and is referred back to in
--   -- cycles.
--   --
--   -- 2. If the trace encounters some world w such that the model's termination condition is met, we must show that some
--   -- world in the trace satisfies the eventually of p, otherwise we know that we can safely emit an exception that the
--   -- invariant eventually of p was violated.
--   --
--   -- 3. If the trace enters into a cycle, we know that some world within the cycle must satisfy eventually of p.
--   -- Otherwise, we know it is false since all extensions of the trade imply @'eventually' p == False@
--   result <- checkFormula formula
--   if result ^. isSatisfied
--     then do
--       infoHere . subformula name .= True
--       return result
--     else do
--       infoHere . subformula name .= False
--       return $
--         result
--           & isSatisfied .~ True
--           & isComplete .~ False

-- -- | Check that the formulas @p@, @q@ for some @'upUntil' p q@ are satisfied for the previously unseen world currently
-- -- being inspected by the model checker.
-- --
-- -- @since 0.1.0.0
-- checkUpUntil :: Specifiable ctx => Int -> Term Bool -> Term Bool -> Prop ctx CheckResult
-- checkUpUntil name leftFormula rightFormula = do
--   -- 'upUntil' is checked just as 'eventually' is, except for in addition to these checks we must show that for some
--   -- @'upUntil' p q@, p is true when q is not.
--   leftResult <- checkFormula leftFormula
--   rightResult <- checkFormula rightFormula
--   if rightResult ^. isSatisfied
--     then do
--       infoHere . subformula name .= True
--       return (rightResult & isComplete .~ True)
--     else do
--       unless (leftResult ^. isSatisfied) (throwPropertyViolation name ModalUpUntil)
--       infoHere . subformula name .= False
--       return rightResult

-- -- ---------------------------------------------------------------------------------------------------------------------

-- checkFormulaCyclic :: Specifiable ctx => Behavior ctx -> Term Bool -> Prop ctx CheckResult
-- checkFormulaCyclic cyclicBehavior = \case
--   Conjunct left right -> do
--     leftResult <- checkFormulaCyclic cyclicBehavior left
--     rightResult <- checkFormulaCyclic cyclicBehavior right
--     return (CheckResult (leftResult ^. isSatisfied && rightResult ^. isSatisfied) True unitZero)
--   Disjunct left right ->
--     view disjunctionPath >>= \case
--       Junctions (LeftBranch : js) ->
--         local (set disjunctionPath (Junctions js)) (checkFormulaCyclic cyclicBehavior left)
--       Junctions (RightBranch : js) ->
--         local (set disjunctionPath (Junctions js)) (checkFormulaCyclic cyclicBehavior right)
--       Junctions [] -> undefined -- TODO should be impossible but a made up exception here for when a disjunction is
--       -- encountered but the junction stack is empty

-- checkModalCyclic :: Specifiable ctx => Behavior ctx -> Term Bool -> Prop ctx CheckResult
-- checkModalCyclic cyclicBehavior = \case
--   Always name _ -> checkAlwaysCyclic name
--   Eventually name _ -> checkEventuallyCyclic name cyclicBehavior
--   UpUntil name leftFormula _ -> checkUpUntilCyclic name cyclicBehavior leftFormula
--   InfinitelyOften name formula -> do
--     sat <- checkExpressionCyclic formula
--     infoHere . subformula name .= sat
--     return (CheckResult sat True unitZero)
--   StaysAs name formula -> do
--     sat <- checkExpressionCyclic formula
--     infoHere . subformula name .= sat
--     return (CheckResult sat True unitZero)

-- -- | Check whether an (assumed to be 'always') qualified formula is satisfied within the context of a cyclic trace
-- -- for the world currently being inspected by the model checker.
-- --
-- -- @since 0.1.0.0
-- checkAlwaysCyclic :: Specifiable ctx => Int -> Prop ctx CheckResult
-- checkAlwaysCyclic name = do
--   satHere <- use (infoHere . subformula name)
--   satThere <- use (infoThere . subformula name)
--   unless (satHere && satThere) (throwCyclicViolation name ModalAlways)
--   return (unitOne & isComplete .~ True)

-- -- | Check whether an (assumed to be 'eventually') qualified formula is satisfied within the context of a cyclic trace
-- -- for the world currently being inspected by the model checker.
-- --
-- -- @since 0.1.0.0
-- checkEventuallyCyclic :: Specifiable ctx => Int -> Behavior ctx -> Prop ctx CheckResult
-- checkEventuallyCyclic name cyclicBehavior = do
--   satHere <- use (infoHere . subformula name)
--   if satHere
--     then return (unitOne & isComplete .~ True)
--     else do
--       doesModel <-
--         modelsEventually name
--           <$> view coverageMap
--           <*> pure cyclicBehavior
--       unless doesModel (throwCyclicNoInfo name ModalEventually)
--       return (unitOne & isComplete .~ True)

-- -- | Check whether an (assumed to be 'upUntil') qualified formula is satisfied within the context of a cyclic trace
-- -- for the world currently being inspected by the model checker.
-- --
-- -- @since 0.1.0.0
-- checkUpUntilCyclic :: Specifiable ctx => Int -> Behavior ctx -> Term Bool -> Prop ctx CheckResult
-- checkUpUntilCyclic name cyclicBehavior formula = do
--   satHere <- use (infoHere . subformula name)
--   if satHere
--     then return (unitOne & isComplete .~ True)
--     else do
--       leftSat <- checkExpressionCyclic formula
--       doesModel <-
--         modelsEventually name
--           <$> view coverageMap
--           <*> pure cyclicBehavior
--       unless (doesModel && leftSat) (throwCyclicNoInfo name ModalUpUntil)
--       return (unitOne & isComplete .~ True)

-- checkExpressionCyclic :: forall ctx. Specifiable ctx => Term Bool -> Prop ctx Bool
-- checkExpressionCyclic = \case
--   Value x -> return x
--   Conjunct lefts rights -> do
--     leftResult <- checkExpressionCyclic lefts
--     rightResult <- checkExpressionCyclic rights
--     return (leftResult && rightResult)
--   Disjunct lefts rights -> do
--     leftResult <- checkExpressionCyclic lefts
--     rightResult <- checkExpressionCyclic rights
--     return (leftResult || rightResult)
--   Complement terms -> not <$> checkExpressionCyclic terms
--   _ -> return True

-- -- ---------------------------------------------------------------------------------------------------------------------

-- throwCyclicNoInfo :: Show (Rec ctx) => Int -> Modality -> Prop ctx a
-- throwCyclicNoInfo name modality = do
--   here <- view worldHere
--   throwError (CyclicException (CyclicNoInfo name modality here))

-- throwPropertyViolation :: Show (Rec ctx) => Int -> Modality -> Prop ctx a
-- throwPropertyViolation name modality = do
--   here <- view worldHere
--   throwError (PropertyViolated name modality here)
-- {-# INLINE throwPropertyViolation #-}

-- throwCyclicViolation :: Show (Rec ctx) => Int -> Modality -> Prop ctx a
-- throwCyclicViolation name modality = do
--   here <- view worldHere
--   there <- view worldThere
--   throwError (CyclicException (CyclicViolation name modality here there))
-- {-# INLINE throwCyclicViolation #-}
