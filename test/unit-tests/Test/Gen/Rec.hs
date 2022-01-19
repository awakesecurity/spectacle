module Test.Gen.Rec
  ( -- * Record Generators
    empty,
    singleton,

    -- * Re-exports
    Rec,
  )
where

import Hedgehog (MonadGen)

import Data.Context (type CNil, type (:<))
import Data.Type.Rec (Name, Rec, type (#))
import qualified Data.Type.Rec as Rec

-- ---------------------------------------------------------------------------------------------------------------------

empty :: MonadGen m => m (Rec CNil)
empty = pure Rec.Nil

singleton :: MonadGen m => Name c -> m a -> m (Rec (c # a :< CNil))
singleton n = fmap \x -> Rec.Con n x Rec.Nil
