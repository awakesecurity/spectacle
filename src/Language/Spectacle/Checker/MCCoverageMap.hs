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
import Data.IntMap.Strict as IntMap (IntMap)
import qualified Data.IntMap.Strict as IntMap
import Data.Maybe (Maybe(Just), maybe, fromMaybe)
import Data.Function
import Data.Int
import Data.Semigroup
import Data.Monoid
import Data.Bool
import Lens.Micro (Lens', lens)
import Data.Kind

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
insert (Fingerprint k) info (MCCoverageMap intmap) = MCCoverageMap (IntMap.alter (Just . maybe info (<> info)) k intmap)

lookup :: Fingerprint -> MCCoverageMap -> Maybe MCWorldInfo
lookup (Fingerprint k) (MCCoverageMap intmap) = IntMap.lookup k intmap

member :: Fingerprint -> MCCoverageMap -> Bool
member (Fingerprint k) (MCCoverageMap intmap) = IntMap.member k intmap

size :: MCCoverageMap -> Int
size (MCCoverageMap xs) = IntMap.size xs
