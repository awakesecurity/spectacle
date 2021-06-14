module Language.Spectacle.Spec.Behavior
  ( Behavior,
    cyclicSuffixOf,
    modelsEventually,
  )
where

import qualified Data.HashMap.Strict as HashMap
import Data.Hashable (Hashable)
import Data.Sequence (Seq, pattern Empty, pattern (:<|), pattern (:|>))
import Lens.Micro ((^.))

import Data.Type.Rec (Rec)
import Language.Spectacle.Spec.Coverage (CoverageMap, subformula)

-- ---------------------------------------------------------------------------------------------------------------------

type Behavior ctx = Seq (Rec ctx)

cyclicSuffixOf :: Eq (Rec ctx) => Rec ctx -> Rec ctx -> Behavior ctx -> Maybe (Behavior ctx)
cyclicSuffixOf here there = \case
  here' :<| there' :<| worlds
    | here == here' && there == there' -> Just worlds
    | otherwise -> cyclicSuffixOf here there (there' :<| worlds)
  _ -> Nothing

modelsEventually :: (Hashable (Rec ctx), Eq (Rec ctx)) => Int -> CoverageMap ctx -> Behavior ctx -> Bool
modelsEventually _ _ Empty = False
modelsEventually name coverage (worlds :|> world) =
  case HashMap.lookup world coverage of
    Nothing -> modelsEventually name coverage worlds
    Just info
      | info ^. subformula name -> True
      | otherwise -> modelsEventually name coverage worlds
