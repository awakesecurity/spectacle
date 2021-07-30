-- | The 'CoverageMap' data type. The coverage map assigns coverage information to the 'Fingerprints' of states being
-- explored by the model checker.
--
-- @since 0.1.0.0
module Language.Spectacle.Checker.CoverageMap
  ( -- * Coverage Maps
    CoverageMap (CoverageMap),

    -- ** Lenses
    coverageInfo,

    -- ** Query
    lookupCoverageMap,
    memberCoverageMap,
    sizeCoverageMap,

    -- ** Insertion
    amendCoverageMap,
  )
where

import Data.Bifunctor (first)
import Data.IntMap.Strict as IntMap (IntMap)
import qualified Data.IntMap.Strict as IntMap
import Data.Maybe (fromMaybe)
import Lens.Micro (Lens', lens)

import Language.Spectacle.Checker.Cover (Cover)
import Language.Spectacle.Checker.Fingerprint (Fingerprint (Fingerprint))

-- ---------------------------------------------------------------------------------------------------------------------

-- | The 'CoverageMap' data type mapping from state 'Fingerprint's to the coverage information associated with that
-- state.
--
-- @since 0.1.0.0
newtype CoverageMap where
  CoverageMap :: IntMap Cover -> CoverageMap

-- | @since 0.1.0.0
instance Semigroup CoverageMap where
  CoverageMap cm1 <> CoverageMap cm2 = CoverageMap (IntMap.unionWith (<>) cm1 cm2)
  {-# INLINE (<>) #-}

-- | @since 0.1.0.0
instance Monoid CoverageMap where
  mempty = CoverageMap IntMap.empty
  {-# INLINE mempty #-}

-- | @since 0.1.0.0
instance Show CoverageMap where
  show (CoverageMap coverage) = show . map (first Fingerprint) . IntMap.toList $ coverage
  {-# INLINE show #-}

-- | Lens for modifying the coverage information associated with the fingerprint parameterizing the lens. If @w@ is a
-- world (or state) with a fingerprint @f@, then the coverage information that has been recorded for @w@ can be quickly
-- modified or queried via @'coverageInfo' f@.
--
-- @since 0.1.0.0
coverageInfo :: Fingerprint -> Lens' CoverageMap Cover
coverageInfo fingerprint = lens (lookupCoverageMap fingerprint) (flip (amendCoverageMap fingerprint))
{-# INLINE coverageInfo #-}

-- | Look up the coverage information associated with the fingerprint of some world. Assuming there is no coverage
-- information associated with the given 'Fingerprint', an empty 'Cover' is returned.
--
-- @since 0.1.0.0
lookupCoverageMap :: Fingerprint -> CoverageMap -> Cover
lookupCoverageMap (Fingerprint fp) (CoverageMap coverage) = fromMaybe mempty (IntMap.lookup fp coverage)
{-# INLINE lookupCoverageMap #-}

-- | Look up if there exists any coverage information associated with the given 'Fingerprint'.
--
-- @since 0.1.0.0
memberCoverageMap :: Fingerprint -> CoverageMap -> Bool
memberCoverageMap (Fingerprint fp) (CoverageMap coverage) = IntMap.member fp coverage
{-# INLINE memberCoverageMap #-}

-- | Query the size of a 'CoverageMap'.
--
-- @since 0.1.0.0
sizeCoverageMap :: CoverageMap -> Int
sizeCoverageMap (CoverageMap coverage) = IntMap.size coverage
{-# INLINE sizeCoverageMap #-}

-- | Insert coverage information associated to a 'Fingerprint' in the given 'CoverageMap'.
--
-- @since 0.1.0.0
amendCoverageMap :: Fingerprint -> Cover -> CoverageMap -> CoverageMap
amendCoverageMap (Fingerprint fp) cover (CoverageMap coverage) = CoverageMap (IntMap.insertWith (<>) fp cover coverage)
{-# INLINE amendCoverageMap #-}
