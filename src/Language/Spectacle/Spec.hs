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
import Language.Spectacle.AST (Action, Initial, Invariant, Terminate, runInitial, runInvariant)
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

doModelCheck ::
  forall ctx.
  Specifiable ctx =>
  Initial ctx () ->
  Action ctx Bool ->
  Invariant ctx Bool ->
  Maybe (Terminate ctx Bool) ->
  Fairness ->
  (Either SpecException [Behavior ctx], ModelState ctx)
doModelCheck initial action invariant terminate fairness = runChecker do
  initialWorlds <- doInitialAction initial
  behaviors <- mapM (\w -> startModel w action invariant terminate fairness) initialWorlds
  return (concat behaviors)

-- | 'doInitialAction' expands the initial action provided to a model into a set of worlds the model checker will
-- start from.
--
-- @since 0.1.0.0
doInitialAction :: forall ctx. Specifiable ctx => Initial ctx () -> Checker ctx [Rec ctx]
doInitialAction initial = case runInitial initial of
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
  terms <- case runInvariant True world world invariant of
    Left exc -> throwError (RuntimeException (mempty :: Behavior ctx) exc)
    Right ts -> return ts

  case filter (/= Junctions []) (nub (paveJunctions terms)) of
    [] -> do
      result <- sendModel world (emptyModelCtx world action invariant terminate mempty fairness)
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

-- sequence <$> forM junctions \junction -> do
--   let modelCtx = emptyModelCtx world' action invariant terminate junction fairness
--   trace (show junction) (pure ())
--   sendModel world' modelCtx

sendModel :: Specifiable ctx => Rec ctx -> ModelCtx ctx -> Checker ctx (Either SpecException [Behavior ctx])
sendModel initialWorld modelCtx = do
  let (result, _) = runModel (ModelState mempty) modelCtx (stepModel initialWorld)
  return result

--     let modelCtx world = emptyModelCtx world action invariant terminate fairness
--         (behaviors, modelState) =
--           mapM (\world -> foldContexts (modelCtx world) world) initialWorlds
--             & flip runState (ModelState HashMap.empty)
--         (excs, xs) = partitionEithers behaviors
--      in if null xs
--            then case excs of
--                   [] -> undefined
--                   x : xs -> trace (concat (map show excs)) (ModelFailure x)
--            else ModelSuccess (concat xs) modelState
--           -- case partitionEithers behaviors of
--           -- (_, x : xs) -> ModelSuccess (concat (x : xs)) modelState
--           -- (exc : _, []) -> ModelFailure exc
-- where
--   foldContexts :: ModelCtx ctx -> Rec ctx -> State (ModelState ctx) (Either SpecException [Behavior ctx])
--   foldContexts modelCtx world = do
--     modelState <- get
--     case runModel modelState modelCtx (modelCheck world) of
--       (Left exc, _) -> return (Left exc)
--       (Right behavior, modelState') -> do
--         put modelState'
--         return (Right behavior)

--   modelCheck :: Rec ctx -> Model ctx [Behavior ctx]
--   modelCheck world = do
--     modelCtx <- ask
--     terms <- view modelFormula >>= either throwRuntimeException pure . runInvariant True world world
--     trace (show terms) (pure ())
--     let junctions = nub (paveJunctions terms)
--         composites = compositeModalities terms
--     concat <$> forM junctions \junction -> do
--       trace (show junction ++ ", but: " ++ show junctions) (pure())
--       let ctx' =
--             modelCtx
--               & compositeChecks .~ composites
--               & modelJunctions .~ junction
--       local (const ctx') (stepModel world)

--   compositeModalities :: Term Bool -> Set (Int, Modality)
--   compositeModalities = \case
--     Conjunct lhs rhs -> compositeModalities lhs <> compositeModalities rhs
--     Disjunct lhs rhs -> compositeModalities lhs <> compositeModalities rhs
--     InfinitelyOften name _ -> Set.singleton (name, ModalInfinitelyOften)
--     StaysAs name _ -> Set.singleton (name, ModalStaysAs)
--     _ -> Set.empty
