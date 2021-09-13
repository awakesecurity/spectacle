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

import Data.Bool (Bool)
import Data.Function ((.))
import Data.Int (Int)
import Data.IntMap.Strict as IntMap (IntMap)
import qualified Data.IntMap.Strict as IntMap
import Data.Kind (Type)
import Data.Maybe (Maybe (Just), maybe)
import Data.Monoid (Monoid (mempty), (<>))
import Data.Semigroup (Semigroup)
import GHC.Real (fromIntegral)

import Language.Spectacle.Checker.Fingerprint (Fingerprint)
import Language.Spectacle.Checker.MCWorldInfo (MCWorldInfo)

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
