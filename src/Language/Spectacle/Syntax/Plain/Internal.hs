{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE UndecidableInstances #-}

module Language.Spectacle.Syntax.Plain.Internal
  ( Plain (Plain),
    Effect (PlainVar),
    PlainIntro,
    plainIntro,
  )
where

import Data.Kind (Constraint, Type)
import Data.Void (Void)
import GHC.TypeLits (Symbol)

import Data.Type.Rec (Name, type (#), type (.|))
import Language.Spectacle.Lang (Effect, EffectK, Lang)

-- -------------------------------------------------------------------------------------------------

newtype Plain :: EffectK where
  Plain :: Void -> Plain a

data instance Effect Plain m a where
  PlainVar :: (m ~ Lang ctx effs, s # a .| ctx) => Name s -> Effect Plain m a

type PlainIntro :: (Type -> Type) -> Symbol -> Type -> Constraint
class PlainIntro m s a where
  plainIntro :: Name s -> m a
