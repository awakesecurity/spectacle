{-# LANGUAGE TypeFamilies #-}

module Language.Spectacle.Syntax.Prime.Internal
  ( Prime (Prime),
    Effect (PrimeVar),
  )
where

import Data.Void (Void)

import Data.Type.Rec (Name, type (#), type (.|))
import Language.Spectacle.Lang (Effect, EffectK, Lang)

-- -------------------------------------------------------------------------------------------------

newtype Prime :: EffectK where
  Prime :: Void -> Prime a

data instance Effect Prime m a where
  PrimeVar :: (m ~ Lang ctx eff, s # a .| ctx) => Name s -> Effect Prime m a
