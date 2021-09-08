{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Language.Spectacle.Syntax.Plain.Internal
  ( Plain (Plain),
    Effect (PlainVar),
    PlainIntro,
    plainIntro,
  )
where

import Data.Kind
import Data.Void (Void)
import GHC.TypeLits

import Data.Type.Rec (Name, type (#), type (.|), Ascribe)
import Language.Spectacle.Lang (Effect, EffectK, Lang, Member, send, scope)
import Data.Context

-- -------------------------------------------------------------------------------------------------

newtype Plain :: EffectK where
  Plain :: Void -> Plain a

data instance Effect Plain m a where
  PlainVar :: (m ~ Lang ctx effs, s # a .| ctx) => Name s -> Effect Plain m a

type PlainIntro :: (Type -> Type) -> Symbol -> Type -> Constraint
class PlainIntro m s a where
  plainIntro :: Name s -> m a
