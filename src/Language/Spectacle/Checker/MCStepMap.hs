-- | State tracking the enabled steps leading from an unwrapped fingerprint.
--
-- @since 0.1.0.0
module Language.Spectacle.Checker.MCStepMap
  ( -- * MCStepMap
    MCStepMap (MCStepMap),
    getMCStepMap,

    -- ** Construction
    empty,
    singleton,

    -- ** Query
    lookupWorld,
    lookupFingerprint,

    -- ** Insertion
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
{-# INLINE singleton #-}

lookupWorld :: World ctxt -> MCStepMap -> Maybe (Set String)
lookupWorld (World fingerprint _) = lookupFingerprint fingerprint
{-# INLINE lookupWorld #-}

lookupFingerprint :: Fingerprint -> MCStepMap -> Maybe (Set String)
lookupFingerprint fingerprint = IntMap.lookup (fromIntegral fingerprint) . coerce
{-# INLINE lookupFingerprint #-}

insertAction :: World ctxt -> String -> MCStepMap -> MCStepMap
insertAction (World fingerprint _) = insertFingerprint fingerprint
{-# INLINE insertAction #-}

insertFingerprint :: Fingerprint -> String -> MCStepMap -> MCStepMap
insertFingerprint fingerprint action (MCStepMap intmap) =
  MCStepMap (IntMap.insertWith Set.union (fromIntegral fingerprint) (Set.singleton action) intmap)
{-# INLINE insertFingerprint #-}

union :: MCStepMap -> MCStepMap -> MCStepMap
union (MCStepMap m1) (MCStepMap m2) = MCStepMap (IntMap.union m1 m2)
{-# INLINE union #-}
