{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TypeFamilies #-}

module Language.Spectacle.Syntax.Closure.Internal
  ( Closure (Closure),
    Effect (Close),
  )
where

import Data.Void (Void)

import Data.Name (Name)
import Data.Type.Rec (Has)
import Language.Spectacle.Lang (Effect, EffectK, Lang, ScopeK)
import Language.Spectacle.RTS.Registers (StateFun)

-- ---------------------------------------------------------------------------------------------------------------------

newtype Closure :: EffectK where
  Closure :: Void -> Closure a

data instance Effect Closure :: ScopeK where
  Close ::
    (Has s a ctx, m ~ Lang ctx effs) =>
    Name s ->
    StateFun ctx a ->
    Effect Closure m ()
