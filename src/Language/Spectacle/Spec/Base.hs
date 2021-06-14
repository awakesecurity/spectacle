{-# LANGUAGE FunctionalDependencies #-}

module Language.Spectacle.Spec.Base
  ( Specifiable,
    Fairness (Unfair, WeaklyFair, StronglyFair),
    HasImpliedFormula (impliedFormula),
  )
where

import Data.Hashable (Hashable)

import Data.Type.Rec (Rec, ReflectRow)
import Lens.Micro (Lens')

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

class HasImpliedFormula a b | a -> b where
  impliedFormula :: Lens' a b
