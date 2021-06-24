{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Language.Spectacle.Spec.Prop.Base
  ( -- * Prop Monad Stack
    Prop (Prop, unProp),
    runProp,

    -- ** Prop State
    PropState (PropState, _infoHere, _infoThere),

    -- *** Lenses
    infoHere,
    infoThere,

    -- ** Prop Contexts
    PropCtx (PropCtx, _coverageMap, _worldHere, _worldThere, _behaviorTrace, _disjunctionPath),

    -- *** Lenses
    worldHere,
    worldThere,
    behaviorTrace,
    disjunctionPath,
  )
where

import Control.Monad.Except (ExceptT, MonadError, runExceptT)
import Control.Monad.Reader (MonadReader, Reader, runReader)
import Control.Monad.State (MonadState, StateT (runStateT))
import Lens.Micro (Lens', SimpleGetter, lens, to, (&))

import Data.Type.Rec (Rec)
import Language.Spectacle.Exception.ModelCheckerException (FormulaException)
import Language.Spectacle.Spec.Behavior (Behavior)
import Language.Spectacle.Spec.Coverage (CoverageInfo, CoverageMap, HasCoverageMap (coverageMap))
import Language.Spectacle.Spec.Zipper (Junctions)

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
