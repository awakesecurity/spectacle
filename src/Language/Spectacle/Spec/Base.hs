{-# LANGUAGE FunctionalDependencies #-}

module Language.Spectacle.Spec.Base
  ( Specifiable,
    Fairness (Unfair, WeaklyFair, StronglyFair),
    Modality (ModalAlways, ModalEventually, ModalUpUntil, ModalStaysAs, ModalInfinitelyOften),
  )
where

import Data.Hashable (Hashable)

import Data.Type.Rec (Rec, ReflectRow)

-- ---------------------------------------------------------------------------------------------------------------------

type Specifiable ctx =
  ( Show (Rec ctx)
  , Ord (Rec ctx)
  , Hashable (Rec ctx)
  , ReflectRow ctx
  )

data Fairness
  = Unfair
  | WeaklyFair
  | StronglyFair
  deriving (Enum, Show, Eq)

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
