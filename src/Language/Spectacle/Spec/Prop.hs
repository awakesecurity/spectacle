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
    FormulaException (CyclicException, PropertyViolated, EvaluatorException),
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
      Junctions [] -> throwError EvaluatorException 
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
