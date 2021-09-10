{-# LANGUAGE NoImplicitPrelude #-}

-- | The 'CoverageMap' data type. The coverage map assigns coverage information to the 'Fingerprints' of states being
-- explored by the model checker.
--
-- @since 0.1.0.0
module Language.Spectacle.Checker.MCCoverageMap
  ( -- * Coverage Maps
    MCCoverageMap (MCCoverageMap),
    getCoverageMap,

    -- ** Insertion
    insert,

    -- ** Query
    lookup,
    member,
    size,
  )
where

import Data.Bifunctor (first)
import Data.Bool
import Data.Function
import Data.Int
import Data.IntMap.Strict as IntMap (IntMap)
import qualified Data.IntMap.Strict as IntMap
import Data.Kind
import Data.Maybe (Maybe (Just), fromMaybe, maybe)
import Data.Monoid
import Data.Semigroup
import GHC.Real (fromIntegral)
import Lens.Micro (Lens', lens)

import Language.Spectacle.Checker.Fingerprint (Fingerprint (Fingerprint))
import Language.Spectacle.Checker.MCWorldInfo

-- ---------------------------------------------------------------------------------------------------------------------

newtype MCCoverageMap :: Type where
  MCCoverageMap :: {getCoverageMap :: IntMap MCWorldInfo} -> MCCoverageMap

-- | @since 0.1.0.0
instance Semigroup MCCoverageMap where
  MCCoverageMap xs <> MCCoverageMap ys = MCCoverageMap (IntMap.unionWith (<>) xs ys)
  {-# INLINE (<>) #-}

-- | @since 0.1.0.0
instance Monoid MCCoverageMap where
  mempty = MCCoverageMap IntMap.empty
  {-# INLINE mempty #-}

insert :: Fingerprint -> MCWorldInfo -> MCCoverageMap -> MCCoverageMap
insert k info (MCCoverageMap intmap) = MCCoverageMap (IntMap.alter (Just . maybe info (<> info)) (fromIntegral k) intmap)

lookup :: Fingerprint -> MCCoverageMap -> Maybe MCWorldInfo
lookup k (MCCoverageMap intmap) = IntMap.lookup (fromIntegral k) intmap

member :: Fingerprint -> MCCoverageMap -> Bool
member k (MCCoverageMap intmap) = IntMap.member (fromIntegral k) intmap

size :: MCCoverageMap -> Int
size (MCCoverageMap xs) = IntMap.size xs
