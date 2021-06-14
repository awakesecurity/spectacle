module Language.Spectacle.Spec.Implication
  ( -- * Implications
    Implication (Implication, _implicationName, _implicatedModality, _expectedTruth),
    complementImplication,

    -- ** Lenses
    implicationName,
    implicatedModality,
    expectedTruth,

    -- ** Construction
    impliedAlways,
    impliedEventually,
    impliedUpUntil,
    impliedStaysAs,
    impliedInfinitelyOften,

    -- * Modalities
    Modality (ModalAlways, ModalEventually, ModalUpUntil, ModalStaysAs, ModalInfinitelyOften),
    hasPartialCorrectness,
  )
where

import Lens.Micro (Lens', lens, (%~), (&))

-- ---------------------------------------------------------------------------------------------------------------------

data Implication = Implication
  { -- | The unique identifier for the implied temporal formula.
    --
    -- @since 0.1.0.0
    _implicationName :: Int
  , -- | The kind of modal operator that was implied.
    --
    -- @since 0.1.0.0
    _implicatedModality :: Modality
  , -- | The expected truth value when checking this implied property.
    --
    -- @since 0.1.0.0
    _expectedTruth :: Bool
  }
  deriving (Eq, Ord, Show)

implicationName :: Lens' Implication Int
implicationName = lens _implicationName \implication x -> implication {_implicationName = x}
{-# INLINE implicationName #-}

implicatedModality :: Lens' Implication Modality
implicatedModality = lens _implicatedModality \implication x -> implication {_implicatedModality = x}
{-# INLINE implicatedModality #-}

expectedTruth :: Lens' Implication Bool
expectedTruth = lens _expectedTruth \implication x -> implication {_expectedTruth = x}
{-# INLINE expectedTruth #-}

-- | Negates the expected truth value of an implication.
--
-- @since 0.1.0.0
complementImplication :: Implication -> Implication
complementImplication implication = implication & expectedTruth %~ not
{-# INLINE complementImplication #-}

-- | Construct an implication for an always-qualified formula given its name.
--
-- @since 0.1.0.0
impliedAlways :: Int -> Implication
impliedAlways name = Implication name ModalAlways True
{-# INLINE CONLIKE impliedAlways #-}

-- | Construct an implication for an eventually-qualified formula given its name.
--
-- @since 0.1.0.0
impliedEventually :: Int -> Implication
impliedEventually name = Implication name ModalEventually True
{-# INLINE CONLIKE impliedEventually #-}

-- | Construct an implication for an until-qualified formula given its name.
--
-- @since 0.1.0.0
impliedUpUntil :: Int -> Implication
impliedUpUntil name = Implication name ModalUpUntil True
{-# INLINE CONLIKE impliedUpUntil #-}

-- | Construct an implication for an stays as-qualified formula given its name.
--
-- @since 0.1.0.0
impliedStaysAs :: Int -> Implication
impliedStaysAs name = Implication name ModalStaysAs True
{-# INLINE CONLIKE impliedStaysAs #-}

-- | Construct an implication for an infinitely often-qualified formula given its name.
--
-- @since 0.1.0.0
impliedInfinitelyOften :: Int -> Implication
impliedInfinitelyOften name = Implication name ModalInfinitelyOften True
{-# INLINE CONLIKE impliedInfinitelyOften #-}

-- | An enumeration of the modalities that can be expressed in a temporal formula.
--
-- @since 0.1.0.0
data Modality
  = ModalAlways
  | ModalEventually
  | ModalUpUntil
  | ModalStaysAs
  | ModalInfinitelyOften
  deriving (Enum, Eq, Ord, Show)

hasPartialCorrectness :: Modality -> Bool
hasPartialCorrectness ModalEventually = False
hasPartialCorrectness ModalUpUntil = False
hasPartialCorrectness ModalStaysAs = False
hasPartialCorrectness _ = True
