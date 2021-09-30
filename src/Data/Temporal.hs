-- |
--
-- @since 0.1.0.0
module Data.Temporal
  ( -- * Relations
    ARel (ARel),
    relation,
    canonical,
    representitive,
    equivalence,
    project,
    unitARel,

    -- * Intervals
    Interval (Interval),
    boundryElem,

    -- * Time
    Time (Time, Inf),
    timeExtract,
  )
where

import Data.Temporal.Family
import Data.Temporal.Interval
import Data.Temporal.Time
