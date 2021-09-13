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

import Data.Ascript (type (#))
import Data.Context (Contextual (Ctxt))
import Data.Name (Name)
import Data.Type.Rec (type (.|))
import Language.Spectacle.Lang (Effect, EffectK, Lang, ScopeK)
import Language.Spectacle.RTS.Registers (StateFun)

-- -------------------------------------------------------------------------------------------------

newtype Closure :: EffectK where
  Closure :: Void -> Closure a

data instance Effect Closure :: ScopeK where
  Close ::
    (m ~ Lang ctxt effs, s # a .| ctxt) =>
    Name s ->
    StateFun ctxt a ->
    Effect Closure m ()

type ClosureIntro :: (Type -> Type) -> Symbol -> Type -> Constraint
class ClosureIntro m s a where
  closureIntro :: Ctxt (m ()) ~ ctxt => Name s -> StateFun ctxt a -> m ()
