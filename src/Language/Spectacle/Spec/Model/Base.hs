{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE UndecidableInstances #-}

module Language.Spectacle.Spec.Model.Base
  ( -- * Model Monad
    Model (Model, unModel),
    runModel,

    -- ** Model State
    ModelState (ModelState, _coverageMap),

    -- ** Model Contexts
    ModelCtx
      ( ModelCtx,
        _modelAction,
        _modelFormula,
        _modelTerminate,
        _modelJunctions,
        _modelFairness,
        _modelTrace,
        _worldHere,
        _impliedFormula,
        _compositeChecks
      ),

    -- *** Lenses
    modelAction,
    modelFormula,
    modelTerminate,
    modelJunctions,
    modelFairness,
    modelTrace,
    worldHere,
    impliedFormula,
    compositeChecks,

    -- *** Construction
    emptyModelCtx,

    -- ** Model Results
    ModelResult (ModelFailure, ModelSuccess),
  )
where

import Control.Monad.Except (ExceptT, MonadError, runExceptT)
import Control.Monad.Reader (MonadReader, Reader, runReader)
import Control.Monad.State.Strict (MonadState, StateT (runStateT))
import Data.HashMap.Strict (HashMap)
import qualified Data.Sequence as Seq
import Data.Set (Set)
import qualified Data.Set as Set
import Lens.Micro (Lens', SimpleGetter, lens, to, (&))

import Data.Type.Rec (Rec)
import Language.Spectacle.AST (Action, Invariant, Terminate, applyRewrites)
import Language.Spectacle.Exception (SpecException)
import Language.Spectacle.Spec.Base (Fairness, HasImpliedFormula (impliedFormula))
import Language.Spectacle.Spec.Behavior (Behavior)
import Language.Spectacle.Spec.Coverage (CoverageInfo, HasCoverageMap (coverageMap))
import Language.Spectacle.Spec.Implication (Implication, Modality)
import Language.Spectacle.Spec.Zipper (Junctions (Junctions))

-- ---------------------------------------------------------------------------------------------------------------------

newtype Model ctx a = Model
  { unModel ::
      ExceptT SpecException (StateT (ModelState ctx) (Reader (ModelCtx ctx))) a
  }
  deriving (Functor, Applicative, Monad)
  deriving
    ( MonadError SpecException
    , MonadState (ModelState ctx)
    , MonadReader (ModelCtx ctx)
    )

runModel :: ModelState ctx -> ModelCtx ctx -> Model ctx a -> (Either SpecException a, ModelState ctx)
runModel modelCheckSt modelCheckCtx model =
  model
    & unModel
    & runExceptT
    & flip runStateT modelCheckSt
    & flip runReader modelCheckCtx
{-# INLINE runModel #-}

-- ---------------------------------------------------------------------------------------------------------------------

newtype ModelState ctx = ModelState
  { _coverageMap ::
      HashMap (Rec ctx) (CoverageInfo ctx)
  }

-- | @since 0.1.0.0
instance Show (Rec ctx) => Show (ModelState ctx) where
  show (ModelState coverage) = show coverage
  {-# INLINE show #-}

-- | @since 0.1.0.0
instance HasCoverageMap ModelState where
  coverageMap = lens _coverageMap \mcSt x -> mcSt {_coverageMap = x}
  {-# INLINE coverageMap #-}

-- ---------------------------------------------------------------------------------------------------------------------

data ModelCtx ctx = ModelCtx
  { _modelAction :: Action ctx Bool
  , _modelFormula :: Invariant ctx Bool
  , _modelTerminate :: Maybe (Terminate ctx Bool)
  , _modelJunctions :: Junctions
  , _modelFairness :: Fairness
  , _modelTrace :: Behavior ctx
  , _worldHere :: Rec ctx
  , _impliedFormula :: Set Implication
  , _compositeChecks :: Set (Int, Modality)
  }

instance Show (Rec ctx) => Show (ModelCtx ctx) where
  show (ModelCtx _ _ _ js fair p here impl composites) =
    "ModelCtx {_modelJunctions = " ++ show js
      ++ "; _modelFairness = "
      ++ show fair
      ++ "; _path = "
      ++ show p
      ++ "; _worldHere = "
      ++ show here
      ++ "; _implied = "
      ++ show impl
      ++ "; _composites = "
      ++ show composites
      ++ "}"

-- | @since 0.1.0.0
instance HasImpliedFormula (ModelCtx ctx) (Set Implication) where
  impliedFormula = lens _impliedFormula \ctx x -> ctx {_impliedFormula = x}
  {-# INLINE impliedFormula #-}

modelAction :: SimpleGetter (ModelCtx ctx) (Action ctx Bool)
modelAction = to _modelAction
{-# INLINE CONLIKE modelAction #-}

modelFormula :: SimpleGetter (ModelCtx ctx) (Invariant ctx Bool)
modelFormula = to _modelFormula
{-# INLINE CONLIKE modelFormula #-}

modelTerminate :: SimpleGetter (ModelCtx ctx) (Maybe (Terminate ctx Bool))
modelTerminate = to _modelTerminate
{-# INLINE CONLIKE modelTerminate #-}

modelJunctions :: Lens' (ModelCtx ctx) Junctions
modelJunctions = lens _modelJunctions \ctx x -> ctx {_modelJunctions = x}
{-# INLINE modelJunctions #-}

modelFairness :: SimpleGetter (ModelCtx ctx) Fairness
modelFairness = to _modelFairness
{-# INLINE CONLIKE modelFairness #-}

modelTrace :: Lens' (ModelCtx ctx) (Behavior ctx)
modelTrace = lens _modelTrace \ctx x -> ctx {_modelTrace = x}
{-# INLINE modelTrace #-}

worldHere :: Lens' (ModelCtx ctx) (Rec ctx)
worldHere = lens _worldHere \ctx x -> ctx {_worldHere = x}
{-# INLINE worldHere #-}

compositeChecks :: Lens' (ModelCtx ctx) (Set (Int, Modality))
compositeChecks = lens _compositeChecks \ctx x -> ctx {_compositeChecks = x}
{-# INLINE CONLIKE compositeChecks #-}

emptyModelCtx ::
  Rec ctx ->
  Action ctx Bool ->
  Invariant ctx Bool ->
  Maybe (Terminate ctx Bool) ->
  Fairness ->
  ModelCtx ctx
emptyModelCtx world action invariant terminate fairness =
  ModelCtx
    { _modelAction = action
    , _modelFormula = applyRewrites invariant
    , _modelTerminate = terminate
    , _modelJunctions = Junctions []
    , _modelFairness = fairness
    , _modelTrace = Seq.singleton world
    , _worldHere = world
    , _impliedFormula = Set.empty
    , _compositeChecks = Set.empty
    }

-- ---------------------------------------------------------------------------------------------------------------------

data ModelResult ctx
  = ModelFailure SpecException
  | ModelSuccess [Behavior ctx] (ModelState ctx)

deriving instance Show (Rec ctx) => Show (ModelResult ctx)
