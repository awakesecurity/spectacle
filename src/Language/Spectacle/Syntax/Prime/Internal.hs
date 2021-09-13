{-# LANGUAGE TypeFamilies #-}

module Language.Spectacle.Syntax.Prime.Internal
  ( Prime (Prime),
    Effect (PrimeVar),
    PrimeIntro,
    primeIntro,
  )
where

import Data.Kind (Constraint, Type)
import Data.Void (Void)
import GHC.TypeLits (Symbol)

import Data.Type.Rec (Name, type (#), type (.|))
import Language.Spectacle.Lang (Effect, EffectK, Lang)

-- -------------------------------------------------------------------------------------------------

newtype Prime :: EffectK where
  Prime :: Void -> Prime a

data instance Effect Prime m a where
  PrimeVar :: (m ~ Lang ctx eff, s # a .| ctx) => Name s -> Effect Prime m a

type PrimeIntro :: (Type -> Type) -> Symbol -> Type -> Constraint
class PrimeIntro m s a where
  primeIntro :: Name s -> m a
