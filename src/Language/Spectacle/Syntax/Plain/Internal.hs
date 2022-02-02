{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE UndecidableInstances #-}

module Language.Spectacle.Syntax.Plain.Internal
  ( Plain (Plain),
    Effect (PlainVar),
  )
where

import Data.Void (Void)

import Data.Type.Rec (Has, Name)
import Language.Spectacle.Lang (Effect, EffectK, Lang)

-- -------------------------------------------------------------------------------------------------

newtype Plain :: EffectK where
  Plain :: Void -> Plain a

data instance Effect Plain m a where
  PlainVar :: (Has s a ctx, m ~ Lang ctx effs) => Name s -> Effect Plain m a
