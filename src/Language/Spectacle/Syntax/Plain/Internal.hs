{-# LANGUAGE TypeFamilies #-}

module Language.Spectacle.Syntax.Plain.Internal
  ( Plain (Plain),
    Effect (PlainVar),
  )
where

import Data.Void (Void)

import Data.Type.Rec (Name, type (#), type (.|))
import Language.Spectacle.Lang (Effect, EffectK, Lang)

-- -------------------------------------------------------------------------------------------------

newtype Plain :: EffectK where
  Plain :: Void -> Plain a

data instance Effect Plain m a where
  PlainVar :: (m ~ Lang ctx effs, s # a .| ctx) => Name s -> Effect Plain m a
