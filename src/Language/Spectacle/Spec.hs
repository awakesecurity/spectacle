{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiWayIf #-}

module Language.Spectacle.Spec where

import Control.Monad.Cont (forM)
import Control.Monad.Except
  ( ExceptT,
    MonadError (throwError),
    runExceptT,
  )
import Control.Monad.State.Strict
  ( MonadState,
    State,
    runState,
  )
import Data.Either (partitionEithers)
import Data.Function ((&))
import Data.List (nub)

import Data.Type.Rec (Rec)
import Language.Spectacle.AST (Action, Initial, Invariant, Terminate, runInitial, runInvariant, applyRewrites)
import Language.Spectacle.Exception (SpecException (ModelCheckerException, RuntimeException))
import Language.Spectacle.Exception.ModelCheckerException (ModelCheckerException (NoInitialStates))
import Language.Spectacle.Spec.Base (Fairness, Specifiable)
import Language.Spectacle.Spec.Behavior (Behavior)
import Language.Spectacle.Spec.Model (stepModel)
import Language.Spectacle.Spec.Model.Base
  ( ModelCtx,
    ModelState (ModelState),
    emptyModelCtx,
    runModel,
  )
import Language.Spectacle.Spec.Zipper (Junctions (Junctions), paveJunctions)

-- ---------------------------------------------------------------------------------------------------------------------

newtype Checker ctx a = Checker
  { unChecker ::
      ExceptT SpecException (State (ModelState ctx)) a
  }
  deriving (Functor, Applicative, Monad)
  deriving (MonadState (ModelState ctx), MonadError SpecException)

runChecker :: Specifiable ctx => Checker ctx a -> (Either SpecException a, ModelState ctx)
runChecker checker =
  checker
    & unChecker
    & runExceptT
    & flip runState (ModelState mempty)

modelCheck ::
  forall ctx.
  Specifiable ctx =>
  Initial ctx () ->
  Action ctx Bool ->
  Invariant ctx Bool ->
  Maybe (Terminate ctx Bool) ->
  Fairness ->
  (Either SpecException [Behavior ctx], ModelState ctx)
modelCheck initial action invariant terminate fairness = runChecker do
  initialWorlds <- generateInitialWorlds initial
  behaviors <- mapM (\w -> startModel w action invariant terminate fairness) initialWorlds
  return (concat behaviors)

-- | 'generateInitialWorlds' expands the initial action provided to a model into a set of worlds the model checker will
-- start from.
--
-- @since 0.1.0.0
generateInitialWorlds :: forall ctx. Specifiable ctx => Initial ctx () -> Checker ctx [Rec ctx]
generateInitialWorlds initial = case runInitial initial of
  Left exc -> throwError (RuntimeException (mempty :: Behavior ctx) exc)
  Right worlds
    | null worlds -> throwError (ModelCheckerException (mempty :: Behavior ctx) NoInitialStates)
    | otherwise -> return (nub worlds)

startModel ::
  forall ctx.
  Specifiable ctx =>
  Rec ctx ->
  Action ctx Bool ->
  Invariant ctx Bool ->
  Maybe (Terminate ctx Bool) ->
  Fairness ->
  Checker ctx [Behavior ctx]
startModel world action invariant terminate fairness = do
  let invariant' = applyRewrites invariant
  terms <- case runInvariant True world world invariant' of
    Left exc -> throwError (RuntimeException (mempty :: Behavior ctx) exc)
    Right ts -> return ts

  case filter (/= Junctions []) (nub (paveJunctions terms)) of
    [] -> do
      result <- sendModel world (emptyModelCtx world action invariant' terminate mempty fairness)
      case result of
        Left exc -> throwError exc
        Right behavior -> return behavior
    j : js -> do
      results <- sendOnJunctions world (j : js)
      return (concat results)
  where
    sendOnJunctions :: Rec ctx -> [Junctions] -> Checker ctx [[Behavior ctx]]
    sendOnJunctions world' junctions = do
      (errors, results) <-
        partitionEithers <$> forM junctions \junction -> do
          let modelCtx = emptyModelCtx world' action invariant terminate junction fairness
          sendModel world' modelCtx
      if null results
        then mapM throwError errors
        else return results

sendModel :: Specifiable ctx => Rec ctx -> ModelCtx ctx -> Checker ctx (Either SpecException [Behavior ctx])
sendModel initialWorld modelCtx = do
  let (result, _) = runModel (ModelState mempty) modelCtx (stepModel initialWorld)
  return result
