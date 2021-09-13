-- | Liveness coverage map, an 'IntSet' wrapper tracking unwrapped 'Fingerprints'.
--
-- @since 0.1.0.0
module Language.Spectacle.Checker.LVStateCoverage
  ( -- * LVStateCoverage
    LVStateCoverage (LVStateCoverage),
    getLVStateCoverage,

    -- * Construction
    empty,

    -- ** Insertion
    insert,
    union,

    -- ** Query
    member,
  )
where

import Data.IntSet (IntSet)
import qualified Data.IntSet as IntSet
import Data.Kind (Type)

import Language.Spectacle.Checker.Fingerprint (Fingerprint)

-- ---------------------------------------------------------------------------------------------------------------------

newtype LVStateCoverage :: Type where
  LVStateCoverage :: {getLVStateCoverage :: IntSet} -> LVStateCoverage
  deriving (Eq, Ord, Show)

empty :: LVStateCoverage
empty = LVStateCoverage IntSet.empty
{-# INLINE CONLIKE empty #-}

insert :: Fingerprint -> LVStateCoverage -> LVStateCoverage
insert fingerprint (LVStateCoverage xs) = LVStateCoverage (IntSet.insert (fromIntegral fingerprint) xs)
{-# INLINE insert #-}

union :: LVStateCoverage -> LVStateCoverage -> LVStateCoverage
union (LVStateCoverage xs) (LVStateCoverage ys) = LVStateCoverage (xs `IntSet.union` ys)
{-# INLINE union #-}

member :: Fingerprint -> LVStateCoverage -> Bool
member fingerprint (LVStateCoverage xs) = IntSet.member (fromIntegral fingerprint) xs
{-# INLINE member #-}
