{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RecordWildCards #-}

module Specifications.DijkstraMutex.Process where

import Data.Hashable (Hashable)
import GHC.Generics (Generic) 
import Numeric.Natural (Natural)

-- ---------------------------------------------------------------------------------------------------------------------

-- | The state associated with each process.
--
-- @since 0.1.0.0
data Process = Process
  { procStep :: StepLabel
  , pix :: ProcessLabel
  , iterState :: [ProcessLabel]
  }
  deriving stock (Eq, Generic, Ord, Show)
  deriving anyclass (Hashable)

-- | The type of process labels to choose from.
newtype ProcessLabel = ProcessLabel Int
  deriving stock (Generic)
  deriving (Enum, Eq, Ord, Show) via Int
  deriving anyclass (Hashable)

-- |
data StepLabel
  = Li0
  | Li1
  | Li2
  | Li3a
  | Li3b
  | Li3c
  | Li4a
  | Li4b
  | Critical
  | Li5
  | Li6
  | Noncritical
  deriving stock (Enum, Eq, Ord, Show, Generic)
  deriving anyclass (Hashable)
