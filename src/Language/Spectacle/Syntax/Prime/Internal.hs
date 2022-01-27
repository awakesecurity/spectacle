{-# LANGUAGE TypeFamilies #-}

module Language.Spectacle.Syntax.Prime.Internal
  ( Prime (Prime),
    Effect (PrimeVar),
  )
where

import Data.Kind (Constraint, Type)
import Data.Void (Void)
import GHC.TypeLits (Symbol)

import Data.Type.Rec (Has, Name)
import Language.Spectacle.Lang (Effect, EffectK, Lang)

-- -------------------------------------------------------------------------------------------------

newtype Prime :: EffectK where
  Prime :: Void -> Prime a

data instance Effect Prime m a where
  PrimeVar :: (Has s a ctx, m ~ Lang ctx eff) => Name s -> Effect Prime m a
