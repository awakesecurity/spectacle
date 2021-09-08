-- |
--
-- @since 0.1.0.0
module Language.Spectacle.Checker.LVStateCoverage
  ( -- *
    LVStateCoverage (LVStateCoverage),
    getLVStateCoverage,
    empty,
    insert,
    union,
    member,
  )
where

import Data.Coerce (coerce)
import Data.IntSet (IntSet)
import qualified Data.IntSet as IntSet
import Data.Kind (Type)

import Language.Spectacle.Checker.Fingerprint (Fingerprint (Fingerprint))

-- ---------------------------------------------------------------------------------------------------------------------

newtype LVStateCoverage :: Type where
  LVStateCoverage :: {getLVStateCoverage :: IntSet} -> LVStateCoverage
  deriving (Eq, Ord, Show)

empty :: LVStateCoverage
empty = LVStateCoverage IntSet.empty
{-# INLINE CONLIKE empty #-}

insert :: Fingerprint -> LVStateCoverage -> LVStateCoverage
insert fingerprint (LVStateCoverage xs) = LVStateCoverage (IntSet.insert (coerce fingerprint) xs)
{-# INLINE insert #-}

union :: LVStateCoverage -> LVStateCoverage -> LVStateCoverage
union (LVStateCoverage xs) (LVStateCoverage ys) = LVStateCoverage (xs `IntSet.union` ys)
{-# INLINE union #-}

member :: Fingerprint -> LVStateCoverage -> Bool
member fingerprint (LVStateCoverage xs) = IntSet.member (coerce fingerprint) xs
{-# INLINE member #-}
