{-# LANGUAGE RecordWildCards #-}

-- | State coverage information.
--
-- @since 0.1.0.0
module Language.Spectacle.Checker.Cover
  ( -- * Cover
    Cover
      ( Cover,
        _succeedingWorlds,
        _livenessProperties,
        _hasBeenExplored
      ),

    -- ** Predicates
    isSuccessor,
    isSuccessorWorld,

    -- ** Lenses
    succeedingWorlds,
    livenessProperties,
    hasBeenExplored,
  )
where

import Data.Hashable (Hashable)
import Data.IntSet (IntSet)
import qualified Data.IntSet as IntSet
import Data.Set (Set)
import qualified Data.Set as Set
import Lens.Micro (Lens', lens, (^.))

import Data.Type.Rec (Rec)
import Language.Spectacle.Checker.Fingerprint (Fingerprint, fingerprintRec)

-- ---------------------------------------------------------------------------------------------------------------------

-- | The 'Cover' data type is the coverage information associated with worlds (or states) the model checker explores.
-- 'Cover' only tracks a worlds relationship with other worlds in a model, what liveness properties have been satisfied
-- by this state.
--
-- @since 0.1.0.0
data Cover = Cover
  { -- | If (- ⟶ -) is a next-state relation on a set of worlds 𝑊 and 𝑤 ∈ W, then '_succedentWorlds' is a collection
    -- of worlds 𝐴 ⊂ 𝑊 such that @∀ 𝑢 ∈ A, 𝑤 ⟶ 𝑢@ holds.
    --
    -- Note that rather than storing the full @Rec ctx@ for the elements in @𝐴@ a unique hash @'Fingerprint' ctx@ is
    -- stored. This still allows subsequent world's coverage information to be queried by their hash without taking a
    -- memory hit from keeping a set of @'Rec' ctx@ in their entirety.
    --
    -- @since 0.1.0.0
    _succeedingWorlds :: Set Fingerprint
  , -- | A list of unique 'Int' names that are bound to liveness properties in a model's formula that have been
    -- satisfied by this state.
    --
    -- @since 0.1.0.0
    _livenessProperties :: {-# UNPACK #-} !IntSet
  , -- | A 'Bool' that is true only when the model checker has finish inspecting the world associated with this coverage
    -- information.
    --
    -- @since 0.1.0.0
    _hasBeenExplored :: {-# UNPACK #-} !Bool
  }
  deriving (Show)

-- | @since 0.1.0.0
instance Semigroup Cover where
  c1 <> c2 =
    Cover
      { _succeedingWorlds = c1 ^. succeedingWorlds <> c2 ^. succeedingWorlds
      , _livenessProperties = c1 ^. livenessProperties <> c2 ^. livenessProperties
      , _hasBeenExplored = c1 ^. hasBeenExplored || c2 ^. hasBeenExplored
      }
  {-# INLINE (<>) #-}

-- | @since 0.1.0.0
instance Monoid Cover where
  mempty =
    Cover
      { _succeedingWorlds = Set.empty
      , _livenessProperties = IntSet.empty
      , _hasBeenExplored = False
      }
  {-# INLINE mempty #-}

-- | Predicate on 'Cover' checking if the fingerprint given has been recorded as a successor world.
--
-- @since 0.1.0.0
isSuccessor :: Fingerprint -> Cover -> Bool
isSuccessor fingerprint cover = Set.member fingerprint (cover ^. succeedingWorlds)
{-# INLINE isSuccessor #-}

-- | Predicate on 'Cover' checking if the world given has been recorded as a successor.
--
-- @since 0.1.0.0
isSuccessorWorld :: Hashable (Rec ctx) => Rec ctx -> Cover -> Bool
isSuccessorWorld world = isSuccessor (fingerprintRec world)
{-# INLINE isSuccessorWorld #-}

-- | If @𝑤 𝑅 -@ is a next-state relation and @𝑤@ is the world associated with the 'Cover' this lens is focusing, then
-- 'succeedingWorlds' is any world @𝑢@ such that @𝑤 𝑅 𝑢@ is enabled.
--
-- @since 0.1.0.0
succeedingWorlds :: Lens' Cover (Set Fingerprint)
succeedingWorlds = lens _succeedingWorlds \Cover {..} x -> Cover {_succeedingWorlds = x, ..}
{-# INLINE succeedingWorlds #-}

-- | A lens focusing on the names bound to liveness properties that this state has satisfied.
--
-- @since 0.1.0.0
livenessProperties :: Lens' Cover IntSet
livenessProperties = lens _livenessProperties \Cover {..} x -> Cover {_livenessProperties = x, ..}
{-# INLINE livenessProperties #-}

-- | A lens focusing on a boolean value indicating if the model checker has already inspected this state.
--
-- @since 0.1.0.0
hasBeenExplored :: Lens' Cover Bool
hasBeenExplored = lens _hasBeenExplored \Cover {..} x -> Cover {_hasBeenExplored = x, ..}
{-# INLINE hasBeenExplored #-}
