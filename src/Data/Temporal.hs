-- | Temporal data types.
--
-- @since 0.1.0.0
module Data.Temporal
  ( -- * Time
    Time (TimeInf),
    getTime,
    pattern Time,
    pattern Inf,
    timeExtract,

    -- * Intervals
    Interval (Interval),
    timeBefore,
    timeAfter,

    -- * Reactive Types
    RSet (RSet, getRSet),
    fromAction,
    intoTime,
    intoInterval,

    -- * Lifted Types
    Lift (Lift),
    liftRenew,
    liftEval,

    -- * Temporal Functors

    -- ** K Functor
    K (InL, InR),
    pureK,
    inR,
    inL,
    unwrapK,
    toGL,
    toF,

    -- *** Universal Properties
    sumK,
    parK,

    -- ** Global Functor
    GL (GL),
    getGL,

    -- *** Natural Transformations
    endoGL,

    -- ** Future Functor
    F (F),
    getF,

    -- *** Natural transformations
    future,
    endoF,
    idealF,
  )
where

import Data.Temporal.Future (F (F), endoF, future, getF, idealF)
import Data.Temporal.Global (GL (GL), endoGL, getGL)
import Data.Temporal.K (K (InL, InR), inL, inR, parK, pureK, sumK, toF, toGL, unwrapK)
import Data.Temporal.RSet
  ( Lift (..),
    RSet (..),
    fromAction,
    intoInterval,
    intoTime,
    liftEval,
    liftRenew,
  )
import Data.Temporal.Time (Interval (Interval), Time (TimeInf), timeExtract, pattern Inf, pattern Time)
