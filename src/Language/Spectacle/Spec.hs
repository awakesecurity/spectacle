{-# LANGUAGE MultiWayIf #-}

module Language.Spectacle.Spec where

import Control.Monad.Cont (forM)
import Control.Monad.Except
  ( ExceptT,
    MonadError (throwError),
    runExceptT,
  )
import Control.Monad.Reader (MonadReader (ask, local))
import Control.Monad.State.Strict
  ( MonadState (get, put),
    State,
    runState,
  )
import Data.Function ((&))
import qualified Data.HashMap.Strict as HashMap
import Data.Set (Set)
import qualified Data.Set as Set
import Lens.Micro ((.~))
import Lens.Micro.Mtl (view)

import Data.List (nub)
import Data.Type.Rec (Rec)
import Language.Spectacle.AST (Action, Initial, Invariant, Terminate, runInitial, runInvariant)
import Language.Spectacle.Exception (SpecException (RuntimeException))
import Language.Spectacle.Spec.Base (Fairness, Specifiable)
import Language.Spectacle.Spec.Behavior (Behavior)
import Language.Spectacle.Spec.Implication (Modality (ModalInfinitelyOften, ModalStaysAs))
import Language.Spectacle.Spec.Model (stepModel, throwRuntimeException)
import Language.Spectacle.Spec.Model.Base
  ( Model,
    ModelCtx,
    ModelResult (ModelFailure, ModelSuccess),
    ModelState (ModelState),
    compositeChecks,
    emptyModelCtx,
    modelFormula,
    modelJunctions,
    runModel,
  )
import Language.Spectacle.Spec.Zipper (paveJunctions)
import Language.Spectacle.Syntax.Modal
  ( LTerm
      ( ConjunctL4,
        DisjunctL4,
        InfinitelyOftenL3,
        ModalL4,
        StaysAsL3
      ),
    Level (L3, L4),
  )

-- ---------------------------------------------------------------------------------------------------------------------

-- | 'doInitialAction' expands the initial action provided to a model into a set of worlds the model checker will
-- start from.
--
-- @since 0.1.0.0
doInitialAction :: Specifiable ctx => Initial ctx () -> Either SpecException [Rec ctx]
doInitialAction initial = case runInitial initial of
  Left exc -> Left (RuntimeException exc)
  Right worlds -> Right (nub worlds)

doModelCheck ::
  forall ctx.
  Specifiable ctx =>
  Initial ctx () ->
  Action ctx Bool ->
  Invariant ctx Bool ->
  Maybe (Terminate ctx Bool) ->
  Fairness ->
  ModelResult ctx
doModelCheck initial action invariant terminate fairness =
  case doInitialAction initial of
    Left exc -> ModelFailure exc
    Right initialWorlds ->
      let modelCtx world = emptyModelCtx world action invariant terminate fairness
          behaviors =
            mapM (\world -> foldContexts (modelCtx world) world) initialWorlds
              & runExceptT
              & flip runState (ModelState HashMap.empty)
       in case behaviors of
            (Left exc, _) -> ModelFailure exc
            (Right xs, modelState) -> ModelSuccess (concat xs) modelState
  where
    foldContexts :: ModelCtx ctx -> Rec ctx -> ExceptT SpecException (State (ModelState ctx)) [Behavior ctx]
    foldContexts modelCtx world = do
      modelState <- get
      case runModel modelState modelCtx (modelCheck world) of
        (Left exc, _) -> throwError exc
        (Right behavior, modelState') -> do
          put modelState'
          return behavior

    modelCheck :: Rec ctx -> Model ctx [Behavior ctx]
    modelCheck world = do
      modelCtx <- ask
      terms <- view modelFormula >>= either throwRuntimeException pure . runInvariant True world world
      let junctions = paveJunctions terms
          composites = compositeModalities terms
      concat <$> forM junctions \junction ->
        let ctx' =
              modelCtx
                & compositeChecks .~ composites
                & modelJunctions .~ junction
         in local (const ctx') (stepModel world)

    compositeModalities :: LTerm 'L4 Bool -> Set (Int, Modality)
    compositeModalities = goL4
      where
        goL4 :: LTerm 'L4 Bool -> Set (Int, Modality)
        goL4 = \case
          ModalL4 modality -> goL3 modality
          ConjunctL4 lhs rhs -> goL4 lhs <> goL4 rhs
          DisjunctL4 lhs rhs -> goL4 lhs <> goL4 rhs

        goL3 :: LTerm 'L3 Bool -> Set (Int, Modality)
        goL3 = \case
          InfinitelyOftenL3 name _ -> Set.singleton (name, ModalInfinitelyOften)
          StaysAsL3 name _ -> Set.singleton (name, ModalStaysAs)
          _ -> Set.empty
