{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE UndecidableInstances #-}

module Language.Spectacle.Checker.Step
  ( -- * Steps
    Step (Step, _stepFrom, _stepTo),

    -- ** Construction
    makeStep,
    makeReflexStep,
    stepHash,
    toStepImage,

    -- ** Predicates
    isReflexiveStep,

    -- ** Lenses
    stepFrom,
    stepTo,

    -- * Step Images
    StepImage (StepImage, _stepImageFrom, _stepImageTo),

    -- ** Lenses
    stepImageFrom,
    stepImageTo,

    -- ** Operations
    traceToSteps,
  )
where

import Data.Hashable (Hashable (hash, hashWithSalt))
import Data.Sequence (Seq, pattern (:<|))
import qualified Data.Sequence as Seq
import Lens.Micro (Lens', lens, (^.))

import Data.Type.Rec (Rec)
import Data.World (World, worldFingerprint)
import Language.Spectacle.Checker.Fingerprint (Fingerprint (Fingerprint))

-- ---------------------------------------------------------------------------------------------------------------------

-- | The 'Step' data type is step given by some next-state relation where '_stepFrom' is the preceding world and
-- '_stepTo' is the succeding world.
--
-- @since 0.1.0.0
data Step spec = Step
  { _stepFrom :: {-# UNPACK #-} !(World spec)
  , _stepTo :: {-# UNPACK #-} !(World spec)
  }
  deriving (Eq)

-- | @since 0.1.0.0
instance Show (Rec spec) => Show (Step spec) where
  show (Step from to) = show from ++ " --> " ++ show to
  {-# INLINE show #-}

-- | @since 0.1.0.0
instance Hashable (Step spec) where
  hash (Step from to) = stepHash (from ^. worldFingerprint) (to ^. worldFingerprint)
  {-# INLINE hash #-}

  hashWithSalt salt (Step from to) = stepHashWithSalt salt (from ^. worldFingerprint) (to ^. worldFingerprint)
  {-# INLINE hashWithSalt #-}

-- | Construct a 'Step' from a pair of worlds @𝑤@, @𝑢@ related by the 𝑅-step @𝑤 𝑅 𝑢@.
--
-- @since 0.1.0.0
makeStep :: World spec -> World spec -> Step spec
makeStep = Step
{-# INLINE CONLIKE makeStep #-}

-- | The function that implements 'hash' for 'Step's. Every 𝑅-step @𝑤 𝑅 𝑢@ can be converted to a hash uniquely
-- identifying a step from @𝑤@ to @𝑢@ by joining the fingerprints for @𝑤@ to @𝑢@.
--
-- @since 0.1.0.0
stepHash :: Fingerprint -> Fingerprint -> Int
stepHash (Fingerprint fp) (Fingerprint fp') = hashWithSalt (fromIntegral fp) fp'
{-# INLINE stepHash #-}

-- | Forgets the concrete values in a 'Step' leaving only the fingerprints of the initial and terminal worlds in a
-- 'StepImage'.
--
-- @since 0.1.0.0
toStepImage :: Step ctx -> StepImage
toStepImage (Step from to) = StepImage (from ^. worldFingerprint) (to ^. worldFingerprint)
{-# INLINE toStepImage #-}

-- | Similar to 'stepHash' but has an additional argument for adding salt into the resulting hash.
--
-- @since 0.1.0.0
stepHashWithSalt :: Int -> Fingerprint -> Fingerprint -> Int
stepHashWithSalt salt (Fingerprint fp) (Fingerprint fp') = hashWithSalt (hashWithSalt salt fp) fp'
{-# INLINE stepHashWithSalt #-}

-- | Construct a reflexive (or stuttering step) 𝑅-step for the world @w@.
--
-- @since 0.1.0.0
makeReflexStep :: World spec -> Step spec
makeReflexStep world = Step world world
{-# INLINE CONLIKE makeReflexStep #-}

-- | Predicate on 'Step' for if @𝑤 𝑅 𝑤@.
--
-- @since 0.1.0.0
isReflexiveStep :: Step spec -> Bool
isReflexiveStep (Step world world') = world == world'
{-# INLINE CONLIKE isReflexiveStep #-}

-- | If @𝑤 𝑅 𝑢@ is a 𝑅-step, then @'stepFrom' (𝑤 𝑅 𝑢) ≡ 𝑤@.
--
-- @since 0.1.0.0
stepFrom :: Lens' (Step spec) (World spec)
stepFrom = lens _stepFrom \Step {..} x -> Step {_stepFrom = x, ..}
{-# INLINE stepFrom #-}

-- | If @𝑤 𝑅 𝑢@ is a 𝑅-step, then @'stepFrom' (𝑤 𝑅 𝑢) ≡ 𝑤@.
--
-- @since 0.1.0.0
stepTo :: Lens' (Step spec) (World spec)
stepTo = lens _stepTo \Step {..} x -> Step {_stepTo = x, ..}
{-# INLINE stepTo #-}

-- ---------------------------------------------------------------------------------------------------------------------

-- | The data type 'StepImage' is a weaker form of 'Step'. While 'Step' is a 𝑅-step between two worlds, 'StepImage' is
-- a step between the fingerprints of two worlds.
--
-- The concrete values found in @𝑤 :: 'World' spec@ are rarely avaliable to the model checker for an arbitrary @𝑤@ which
-- means it isn't always possible to construct a full 'Step'. 'StepImage' is used in situations where the concrete
-- values for @w@ have already been discarded but there is still the need to query information about a specific 𝑅-step
-- in the model's state.
--
-- @since 0.1.0.0
data StepImage = StepImage
  { _stepImageFrom :: {-# UNPACK #-} !Fingerprint
  , _stepImageTo :: {-# UNPACK #-} !Fingerprint
  }
  deriving (Eq)

-- | @since 0.1.0.0
instance Show StepImage where
  show (StepImage from to) = show from ++ " ⟶* " ++ show to
  {-# INLINE show #-}

-- | @since 0.1.0.0
instance Hashable StepImage where
  hash (StepImage to from) = stepHash to from
  {-# INLINE hash #-}

  hashWithSalt salt (StepImage to from) = stepHashWithSalt salt to from
  {-# INLINE hashWithSalt #-}

-- | If @𝑤 𝑅 𝑢@ is a 𝑅-step, then @'stepImageFrom' (𝑤 𝑅 𝑢)@ is the fingerprint of the world @𝑤@.
--
-- @since 0.1.0.0
stepImageFrom :: Lens' StepImage Fingerprint
stepImageFrom = lens _stepImageFrom \StepImage {..} x -> StepImage {_stepImageFrom = x, ..}
{-# INLINE stepImageFrom #-}

-- | If @𝑤 𝑅 𝑢@ is a 𝑅-step, then @'stepImageFrom' (𝑤 𝑅 𝑢)@ is the fingerprint of the world @𝑢@.
--
-- @since 0.1.0.0
stepImageTo :: Lens' StepImage Fingerprint
stepImageTo = lens _stepImageTo \StepImage {..} x -> StepImage {_stepImageTo = x, ..}
{-# INLINE stepImageTo #-}

-- | If the behavior @b@ is a sequence of worlds @𝑤₁ ⟶ 𝑤₂ ⟶ ... ⟶ 𝑤ₙ@, then @'traceToSteps' b@ is a sequence of 𝑅-steps
-- @𝑤₁ 𝑅 𝑤₂@, @𝑤₂ 𝑅 ...@, @... 𝑅 𝑤ₙ@.
--
-- @since 0.1.0.0
traceToSteps :: Seq Fingerprint -> Seq StepImage
traceToSteps trace = case trace of
  w1 :<| w2 :<| wn -> StepImage w1 w2 :<| traceToSteps (w2 :<| wn)
  _ -> Seq.empty
{-# INLINE traceToSteps #-}
