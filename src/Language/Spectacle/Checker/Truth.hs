module Language.Spectacle.Checker.Truth
  ( -- * Truth Map
    TruthMap (TruthMap),

    -- ** Lenses
    stepTruth,
    stepImageTruth,
    stepTruthTable,
    stepImageTruthTable,

    -- ** Query
    lookupStepTruth,
    lookupStepTruth',
    lookupStepImageTruth,
    lookupStepImageTruth',

    -- ** Insertion
    insertStepTruth,
    insertStepImageTruth,

    -- * Truth Meta Information
    TruthTable (TruthTable),

    -- ** Lenses
    formulaTruth,

    -- ** Query
    lookupTruthTable,
    lookupTruthTable',

    -- ** Insertion
    insertTruthTable,
  )
where

import Data.Coerce (coerce)
import Data.Hashable (hash)
import Data.IntMap.Strict as IntMap (IntMap)
import qualified Data.IntMap.Strict as IntMap
import Data.Maybe (fromMaybe)
import Lens.Micro (Lens', lens)

import Language.Spectacle.Checker.Step (Step, StepImage)

-- ---------------------------------------------------------------------------------------------------------------------

-- | The wrapper type 'TruthMap' is mapping from 𝑅-steps to 'TruthTable'. Together these maps assign the satisfaction of
-- a temporal formula for every unique 𝑅-step that can be taken in a model.
--
-- @since 0.1.0.0
newtype TruthMap where
  -- Nominally, the map domain is 'Int' to minimize space usage; however, the wrapper is practically mapping values of
  -- @hash (x :: Step)@. The wrapper guards against direct insertion of arbitrary 'Int' by only insertion of some
  -- 'Step', and handling the hashing internally.
  TruthMap :: IntMap TruthTable -> TruthMap
  deriving (Show)

-- | @since 0.1.0.0
instance Semigroup TruthMap where
  TruthMap tm1 <> TruthMap tm2 = TruthMap (IntMap.unionWith (<>) tm1 tm2)
  {-# INLINE (<>) #-}

-- | @since 0.1.0.0
instance Monoid TruthMap where
  mempty = TruthMap IntMap.empty
  {-# INLINE CONLIKE mempty #-}

-- | Lens focusing on the truth assignment for the formula and step parameterizing this lens.
--
-- @since 0.1.0.0
stepTruth :: Step spec -> Int -> Lens' TruthMap Bool
stepTruth step name = stepTruthTable step . formulaTruth name
{-# INLINE stepTruth #-}

-- | Like 'stepTruth', but operates over 'StepImage'.
--
-- @since 0.1.0.0
stepImageTruth :: StepImage -> Int -> Lens' TruthMap Bool
stepImageTruth step name = lens (lookupStepImageTruth step) (flip (insertStepImageTruth step)) . formulaTruth name
{-# INLINE stepImageTruth #-}

-- | Lens focusing on the truth table associated with the step paramterizing this lens.
--
-- @since 0.1.0.0
stepTruthTable :: Step step -> Lens' TruthMap TruthTable
stepTruthTable step = lens (lookupStepTruth step) (flip (insertStepTruth step))
{-# INLINE stepTruthTable #-}

-- | Like 'stepTruthTable', but operates over 'StepImage'.
--
-- @since 0.1.0.0
stepImageTruthTable :: StepImage -> Lens' TruthMap TruthTable
stepImageTruthTable step = lens (lookupStepImageTruth step) (flip (insertStepImageTruth step))
{-# INLINE stepImageTruthTable #-}

-- | Queries the truth table associated with a given 𝑅-step represented by a 'Step', giving back the default value
-- @'mempty' :: 'TruthTable'@ if there is no truth table associated with the 𝑅-step.
--
-- @since 0.1.0.0
lookupStepTruth :: Step spec -> TruthMap -> TruthTable
lookupStepTruth step truthMap = fromMaybe mempty (lookupStepTruth' step truthMap)
{-# INLINE lookupStepTruth #-}

-- | Queries the truth table associated with a given 𝑅-step represented by a 'Step'....
--
-- @since 0.1.0.0
lookupStepTruth' :: Step spec -> TruthMap -> Maybe TruthTable
lookupStepTruth' step (TruthMap truthMap) = IntMap.lookup (hash step) truthMap
{-# INLINE lookupStepTruth' #-}

-- | Queries the truth table associated with a given 𝑅-step represented by a 'StepImage'.
--
-- @since 0.1.0.0
lookupStepImageTruth :: StepImage -> TruthMap -> TruthTable
lookupStepImageTruth step truthMap = fromMaybe mempty (lookupStepImageTruth' step truthMap)
{-# INLINE lookupStepImageTruth #-}

-- | Queries the truth table associated with a given 𝑅-step represented by a 'StepImage', giving back the default value
-- @'mempty' :: 'TruthTable'@ if there is no truth table associated with the 𝑅-step.
--
-- @since 0.1.0.0
lookupStepImageTruth' :: StepImage -> TruthMap -> Maybe TruthTable
lookupStepImageTruth' step (TruthMap truthMap) = IntMap.lookup (hash step) truthMap
{-# INLINE lookupStepImageTruth' #-}

-- | Associates an 𝑅-step, represented by a 'Step', and a truth table in the given 'TruthMap'.
--
-- @since 0.1.0.0
insertStepTruth :: Step spec -> TruthTable -> TruthMap -> TruthMap
insertStepTruth step table (TruthMap truthMap)
  | IntMap.null (coerce table :: IntMap Bool) = TruthMap truthMap
  | otherwise = TruthMap (IntMap.adjust (<> table) (hash step) truthMap)
{-# INLINE insertStepTruth #-}

-- | Associates an 𝑅-step, represented by 'StepImage', and a truth table in the given 'TruthMap'.
--
-- @since 0.1.0.0
insertStepImageTruth :: StepImage -> TruthTable -> TruthMap -> TruthMap
insertStepImageTruth step table (TruthMap truthMap)
  | IntMap.null (coerce table :: IntMap Bool) = TruthMap truthMap
  | otherwise = TruthMap (IntMap.alter (Just . maybe mempty (<> table)) (hash step) truthMap)
{-# INLINE insertStepImageTruth #-}

-- | The wrapper type 'TruthTable' which maps the 'FormulaNames' in a temporal formula to a boolean truth-value
-- indicating if that formula with keyed name is satisfied or not. If @(- 𝑅 -)@ is a next-state relation along with some
-- temporal formula @𝑝@ then a 'TruthTable' is assigned to each 𝑅-step in order to track if @𝑝@ on @𝑤 𝑅 𝑢@ for any worlds
-- @𝑤, 𝑢@.
--
-- @since 0.1.0.0
newtype TruthTable where
  -- As is truth with 'TruthMap', we are only mapping 'Int's to 'Bool's here but 'TruthTable' wraps 'IntMap' to ensure
  -- that only hashes resulting from @'hash' (x :: 'FormulaName')@ are ever recorded in the map.
  TruthTable :: IntMap Bool -> TruthTable
  deriving (Show)

-- | @since 0.1.0.0
instance Semigroup TruthTable where
  TruthTable tm1 <> TruthTable tm2 = TruthTable (IntMap.unionWith (||) tm1 tm2)
  {-# INLINE (<>) #-}

-- | @since 0.1.0.0
instance Monoid TruthTable where
  mempty = TruthTable IntMap.empty
  {-# INLINE CONLIKE mempty #-}

-- | The lens @'formulaTruth' name@ focuses on the truth assignment for the formula bound to @name@.
--
-- @since 0.1.0.0
formulaTruth :: Int -> Lens' TruthTable Bool
formulaTruth name = lens (lookupTruthTable name) (flip (insertTruthTable name))
{-# INLINE formulaTruth #-}

-- | 'lookupTruthTable' queries a 'TruthTable' for the truth values assigned to the given 'FormulaName'. If there is no
-- truth value assigned to the given 'FormulaName' the result is always 'False'.
--
-- Use 'lookupTruthTable'' for a version that returns 'Nothing' in the case where there is no truth value associated to
-- the 'FormulaName'.
--
-- @since 0.1.0.0
lookupTruthTable :: Int -> TruthTable -> Bool
lookupTruthTable name truthMeta = fromMaybe False (lookupTruthTable' name truthMeta)
{-# INLINE lookupTruthTable #-}

-- | 'lookupTruthTable' but does not default to 'False' in the case where no truth value is associated with the given
-- 'FormulaName'.
--
-- @since 0.1.0.0
lookupTruthTable' :: Int -> TruthTable -> Maybe Bool
lookupTruthTable' name (TruthTable truthMeta) = IntMap.lookup name truthMeta
{-# INLINE lookupTruthTable' #-}

-- | 'insertTruthTable' adjusts the truthfulness of the formula named by some boolean value. The insertion is equivalent
-- to 'IntMap.adjust (|| x)' to make the insertion idempotent.
--
-- @since 0.1.0.0
insertTruthTable :: Int -> Bool -> TruthTable -> TruthTable
insertTruthTable name truth (TruthTable table) =
  -- The idea behind ignoring insertion for 'False' is that, since truth table insertion is done by (||) to keep updates
  -- monotone, inserting 'False' will never change the utility of the truth table so it's dropped entirely. This is more
  -- preformant since it keeps 'IntMap' traversals to a minimum, but it also guards against memory being allocated  for
  -- useless boolean-values in the 'Intmap'.
  if truth
    then TruthTable (IntMap.insert name True table)
    else TruthTable table
{-# INLINE insertTruthTable #-}
