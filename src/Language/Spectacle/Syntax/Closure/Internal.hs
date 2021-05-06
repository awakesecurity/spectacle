{-# LANGUAGE TypeFamilies #-}

module Language.Spectacle.Syntax.Closure.Internal
  ( Closure (Closure),
    Effect (Close),
  )
where

import Data.Void (Void)

import Data.Ascript (type (#))
import Data.Name (Name)
import Data.Type.Rec (type (.|))
import Language.Spectacle.Lang (Effect, EffectK, Lang, ScopeK)

-- -------------------------------------------------------------------------------------------------

newtype Closure :: [EffectK] -> EffectK where
  Closure :: Void -> Closure sig a

data instance Effect (Closure sig) :: ScopeK where
  Close ::
    (m ~ Lang ctx effs, s # a .| ctx) =>
    Name s ->
    Lang ctx sig a ->
    Effect (Closure sig) m Bool
