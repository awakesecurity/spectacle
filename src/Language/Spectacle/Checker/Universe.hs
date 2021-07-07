{-# LANGUAGE RecordWildCards #-}

module Language.Spectacle.Checker.Universe
  ( -- * The Universe Type
    Universe
      ( Universe,
        _worldCoverage,
        _truthCoverage
      ),

    -- ** Lenses
    worldCoverage,
    truthCoverage,
    modelDepth,
  )
where

import Lens.Micro (Lens', lens, (^.))

import Language.Spectacle.Checker.CoverageMap (CoverageMap)
import Language.Spectacle.Checker.Truth (TruthMap)

-- ---------------------------------------------------------------------------------------------------------------------

-- | The 'Universe' data type is the complete state held by the model checker. It is responsible for tracking the
-- coverage information for all worlds the model checker explores as well as the truth-assignments of temporal formula
-- for all steps defined by a model's next-state relation.
--
-- @since 0.1.0.0
data Universe = Universe
  { _worldCoverage :: CoverageMap
  , _truthCoverage :: TruthMap
  , _modelDepth :: {-# UNPACK #-} !Int
  }
  deriving (Show)

-- | @since 0.1.0.0
instance Semigroup Universe where
  uni1 <> uni2 =
    Universe
      { _worldCoverage = uni1 ^. worldCoverage <> uni2 ^. worldCoverage
      , _truthCoverage = uni1 ^. truthCoverage <> uni2 ^. truthCoverage
      , _modelDepth = max (uni1 ^. modelDepth) (uni2 ^. modelDepth)
      }
  {-# INLINE (<>) #-}

-- | @since 0.1.0.0
instance Monoid Universe where
  mempty = Universe mempty mempty 0
  {-# INLINE mempty #-}

-- | Lens focusing on the 'CoverageMap' held by a 'Universe'.
--
-- @since 0.1.0.0
worldCoverage :: Lens' Universe CoverageMap
worldCoverage = lens _worldCoverage \Universe {..} x -> Universe {_worldCoverage = x, ..}
{-# INLINE CONLIKE worldCoverage #-}

-- | Lens focusing on the 'TruthMap' held by a 'Universe'.
--
-- @since 0.1.0.0
truthCoverage :: Lens' Universe TruthMap
truthCoverage = lens _truthCoverage \Universe {..} x -> Universe {_truthCoverage = x, ..}
{-# INLINE CONLIKE truthCoverage #-}

-- | Lens focusing on the model depth held by a 'Universe'.
--
-- @since 0.1.0.0
modelDepth :: Lens' Universe Int
modelDepth = lens _modelDepth \Universe {..} x -> Universe {_modelDepth = x, ..}
{-# INLINE CONLIKE modelDepth #-}
