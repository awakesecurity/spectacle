{-# LANGUAGE RecordWildCards #-}

module Language.Spectacle.Checker.Metrics
  ( -- * Model Metrics
    ModelMetrics (ModelMetrics, _distinctStates, _treeDepth, _treeWidth),

    -- ** Lenses
    distinctStates,
    treeDepth,
    treeWidth,
  )
where

import Lens.Micro (Lens', lens, (^.))

-- ---------------------------------------------------------------------------------------------------------------------

-- | The 'ModelMetrics' data type packages useful metrics about the result of model checking a specification.
--
-- @since 0.1.0.0
data ModelMetrics = ModelMetrics
  { -- | The number of distinct states discoverd by the model checker.
    _distinctStates :: {-# UNPACK #-} !Int
  , -- | The maximum search depth explored by the model checker.
    _treeDepth :: {-# UNPACK #-} !Int
  , -- | The widest subtree among all subtrees in the search tree explored by the model checker.
    _treeWidth :: {-# UNPACK #-} !Int
  }
  deriving (Show)

-- | @since 0.1.0.0
instance Semigroup ModelMetrics where
  m1 <> m2 =
    ModelMetrics
      { _distinctStates = max (m1 ^. distinctStates) (m2 ^. distinctStates)
      , _treeDepth = max (m1 ^. treeDepth) (m2 ^. treeDepth)
      , _treeWidth = max (m1 ^. treeWidth) (m2 ^. treeWidth)
      }
  {-# INLINE (<>) #-}

-- | @since 0.1.0.0
instance Monoid ModelMetrics where
  mempty = ModelMetrics 0 0 0
  {-# INLINE CONLIKE mempty #-}

-- | A lens focusing on the number of unique states discovered by the model checker.
--
-- @since 0.1.0.0
distinctStates :: Lens' ModelMetrics Int
distinctStates = lens _distinctStates \ModelMetrics {..} x -> ModelMetrics {_distinctStates = x, ..}
{-# INLINE distinctStates #-}

-- | A lens focusing on the maximum search depth explored in a model's state graph.
--
-- @since 0.1.0.0
treeDepth :: Lens' ModelMetrics Int
treeDepth = lens _treeDepth \ModelMetrics {..} x -> ModelMetrics {_treeDepth = x, ..}
{-# INLINE treeDepth #-}

-- | A lens focusing on the maximal state space diameter that occured in the model's state graph.
--
-- @since 0.1.0.0
treeWidth :: Lens' ModelMetrics Int
treeWidth = lens _treeWidth \ModelMetrics {..} x -> ModelMetrics {_treeWidth = x, ..}
{-# INLINE treeWidth #-}
