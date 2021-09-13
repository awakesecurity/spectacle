{-# LANGUAGE RecordWildCards #-}

-- |
--
-- @since 0.1.0.0
module Language.Spectacle.Checker.MCTrace
  ( -- *
    MCTrace (MCTrace),
    _mcTraceActionsTaken,

    -- ** Lenses
    mcTraceActionsTaken,
    mcTraceActionsTakenFrom,
    mcTraceEventuallysTaken,
  )
where

import Data.Maybe (fromMaybe)
import Data.Set (Set)
import qualified Data.Set as Set
import Lens.Micro (Lens', SimpleGetter, lens, to)

import Data.World (World)
import Language.Spectacle.Checker.MCStepMap (MCStepMap)
import qualified Language.Spectacle.Checker.MCStepMap as MCStepMap

-- ---------------------------------------------------------------------------------------------------------------------

data MCTrace = MCTrace
  { _mcTraceActionsTaken :: MCStepMap
  , _mcTraceEventuallysTaken :: Set String
  }
  deriving (Eq, Ord)

-- | @since 0.1.0.0
instance Semigroup MCTrace where
  MCTrace acts1 fs1 <> MCTrace acts2 fs2 = MCTrace (MCStepMap.union acts1 acts2) (Set.union fs1 fs2)
  {-# INLINE (<>) #-}

-- | @since 0.1.0.0
instance Monoid MCTrace where
  mempty = MCTrace MCStepMap.empty Set.empty
  {-# INLINE CONLIKE mempty #-}

mcTraceActionsTaken :: Lens' MCTrace MCStepMap
mcTraceActionsTaken =
  lens
    _mcTraceActionsTaken
    \MCTrace {..} x -> MCTrace {_mcTraceActionsTaken = MCStepMap.union x _mcTraceActionsTaken, ..}
{-# INLINE CONLIKE mcTraceActionsTaken #-}

mcTraceEventuallysTaken :: Lens' MCTrace (Set String)
mcTraceEventuallysTaken =
  lens
    _mcTraceEventuallysTaken
    \MCTrace {..} x -> MCTrace {_mcTraceEventuallysTaken = Set.union x _mcTraceEventuallysTaken, ..}

mcTraceActionsTakenFrom :: World ctxt -> SimpleGetter MCTrace (Set String)
mcTraceActionsTakenFrom world = mcTraceActionsTaken . to (fromMaybe Set.empty . MCStepMap.lookupWorld world)
