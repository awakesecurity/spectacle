{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TypeFamilies #-}

module Language.Spectacle.Syntax.Closure.Internal
  ( Closure (Closure),
    Effect (Close),
    ClosureIntro,
    closureIntro,
  )
where

import Data.Kind (Constraint, Type)
import Data.Void (Void)
import GHC.TypeLits (Symbol)

import Data.Context (Contextual (Ctxt))
import Data.Name (Name)
import Data.Type.Rec (Has)
import Language.Spectacle.Lang (Effect, EffectK, Lang, ScopeK)
import Language.Spectacle.RTS.Registers (StateFun)

-- ---------------------------------------------------------------------------------------------------------------------

newtype Closure :: EffectK where
  Closure :: Void -> Closure a

data instance Effect Closure :: ScopeK where
  Close :: (Has s a ctx, m ~ Lang ctx effs) => Name s -> StateFun ctx a -> Effect Closure m ()

type ClosureIntro :: (Type -> Type) -> Symbol -> Type -> Constraint
class ClosureIntro m s a where
  closureIntro :: Ctxt (m ()) ~ ctx => Name s -> StateFun ctx a -> m ()
