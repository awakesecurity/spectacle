-- |
--
-- @since 0.1.0.0
module Language.Spectacle.Checker.MCStepMap
  ( -- *
    MCStepMap (MCStepMap),
    getMCStepMap,

    -- **
    empty,
    singleton,

    -- **
    lookupWorld,
    lookupFingerprint,

    -- **
    insertAction,
    insertFingerprint,
    union,
  )
where

import Data.Kind ( Type )
import Data.World ( World(World) )
import Data.Coerce ( coerce )
import Data.Set (Set)
import qualified Data.Set as Set
import Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict as IntMap

import Language.Spectacle.Checker.Fingerprint ( Fingerprint )

-- ---------------------------------------------------------------------------------------------------------------------

newtype MCStepMap :: Type where
  MCStepMap :: {getMCStepMap :: IntMap (Set String)} -> MCStepMap
  deriving (Eq, Ord)

empty :: MCStepMap
empty = MCStepMap IntMap.empty
{-# INLINE CONLIKE empty #-}

singleton :: World ctxt -> String -> MCStepMap
singleton (World fingerprint _) action = MCStepMap (IntMap.singleton (fromIntegral fingerprint) (Set.singleton action))

lookupWorld :: World ctxt -> MCStepMap -> Maybe (Set String)
lookupWorld (World fingerprint _) = lookupFingerprint fingerprint

lookupFingerprint :: Fingerprint -> MCStepMap -> Maybe (Set String)
lookupFingerprint fingerprint = IntMap.lookup (fromIntegral fingerprint) . coerce

insertAction :: World ctxt -> String -> MCStepMap -> MCStepMap
insertAction (World fingerprint _) = insertFingerprint fingerprint

insertFingerprint :: Fingerprint -> String -> MCStepMap -> MCStepMap
insertFingerprint fingerprint action (MCStepMap intmap) =
  MCStepMap (IntMap.insertWith Set.union (fromIntegral fingerprint) (Set.singleton action) intmap)

union :: MCStepMap -> MCStepMap -> MCStepMap
union (MCStepMap m1) (MCStepMap m2) = MCStepMap (IntMap.union m1 m2)
