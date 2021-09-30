-- |
--
-- @since 0.1.0.0
module Control.Temporal
  ( -- * Temporal Functor
    Temporal (Temporal),
    runTemporal,

    -- ** Operations
    temporal,
    obsTemporal,
    temporalCoalgebra,

    -- * Observations
    Observation (Obs),
    obsRelation,
    obsPast,
    obsPresent,
    obsFuture,
    obsPrefix,
    obsSuffix,

    -- * Behavior Prefixes
    Prefix (Prefix),
    prefRelation,
    prefHere,
    prefThere,

    -- * Behavior Suffix
    Suffix (Suffix),
    sufRelation,
    sufHere,
    sufThere,
  )
where

import Control.Comonad (Comonad (extract))
import Control.Comonad.Trans.Cofree (CofreeF ((:<)), CofreeT (runCofreeT), coiterT)
import Data.Kind (Type)
import Data.Set (Set)
import qualified Data.Set as Set

import Data.Temporal (ARel (ARel, relation, representitive), Time (Inf, Time))

-- ---------------------------------------------------------------------------------------------------------------------

-- | 'Temporal' is the basis for all temporal functors.
--
-- @since 0.1.0.0
newtype Temporal :: (Type -> Type) -> Type -> Type -> Type -> Type where
  Temporal :: {runTemporal :: CofreeT m (ARel a i) b} -> Temporal m i a b
  deriving (Functor)

temporal :: Applicative f => ARel (Set a) i (Set a) -> Temporal f i (Set a) (Set a)
temporal = Temporal . coiterT (pure . temporalCoalgebra)

-- | Observe a the current step of 'Temporal'.
--
-- @since 0.1.0.0
obsTemporal :: Applicative f => Temporal f i (Set a) (Set a) -> f (Observation f i (Set a))
obsTemporal (Temporal ts) =
  let step = runCofreeT ts
   in case (representitive step, extract step) of
        (t, to :< to') -> Obs (relation step) t to . Temporal <$> to'

-- | Temporal coalgebra generating behaviors of type @Set a@.
--
-- @since 0.1.0.0
temporalCoalgebra :: ARel (Set a) i (Set a) -> ARel (Set a) i (Set a)
temporalCoalgebra (ARel idx rel rep) = case rep of
  Inf -> ARel idx (const Set.empty) Inf
  Time ws -> ARel idx rel (Time (rel (Time ws)))

-- ---------------------------------------------------------------------------------------------------------------------

-- | An 'Observation' record the data for any given step taken by an action.
--
-- * The 'obsRelation' field records the name of the action taken to generate the values of this observation. If we view
--   some action @A@ as a relation between time-varying values @a@, then an A-step relates 'obsPast' with 'obsPresent',
--   and 'obsPresent' with the head of the behavior 'obsFuture'.
--
-- * The 'obsPast' field is a set of "past" or unprimed values.
--
-- * The 'obsPresent' field is the set of "present" or primed values.
--
-- * The 'obsFuture' field is a possibly infinite behavior of A-steps. No information about the states inhabiting
--   'obsFuture' is known until we extract them from the inner 'ARel' comonad.
--
-- @since 0.1.0.0
type Observation :: (Type -> Type) -> Type -> Type -> Type
data Observation f i a = Obs
  { obsRelation :: i
  , obsPast :: Time a
  , obsPresent :: a
  , obsFuture :: Temporal f i a a
  }

-- | Project an observation's prefix.
--
-- @since 0.1.0.0
obsPrefix :: Observation f i a -> Prefix i a
obsPrefix obs = Prefix (obsRelation obs) (obsPast obs) (obsPresent obs)
{-# INLINE obsPrefix #-}

-- | Project an observation's suffix.
--
-- @since 0.1.0.0
obsSuffix :: Observation f i a -> Suffix f i a
obsSuffix obs = Suffix (obsRelation obs) (obsPresent obs) (obsFuture obs)
{-# INLINE obsSuffix #-}

data Prefix i a = Prefix
  { prefRelation :: i
  , prefHere :: Time a
  , prefThere :: a
  }

data Suffix f i a = Suffix
  { sufRelation :: i
  , sufHere :: a
  , sufThere :: Temporal f i a a
  }
