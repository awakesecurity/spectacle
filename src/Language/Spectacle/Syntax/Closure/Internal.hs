{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TypeFamilies #-}

module Language.Spectacle.Syntax.Closure.Internal
  ( ClosureKind (ActionClosure, InitialClosure),
    Closure (Closure),
    Effect (CloseAction, CloseInitial),
  )
where

import Data.Void (Void)

import Data.Ascript (type (#))
import Data.Name (Name)
import Data.Type.Rec (type (.|))
import Language.Spectacle.Lang (Effect, EffectK, Lang, ScopeK)
import Language.Spectacle.RTS.Registers (RelationTerm)
import Language.Spectacle.Syntax.NonDet (NonDet)

-- -------------------------------------------------------------------------------------------------

data ClosureKind = ActionClosure | InitialClosure

newtype Closure :: ClosureKind -> EffectK where
  Closure :: Void -> Closure k a

data instance Effect (Closure 'ActionClosure) :: ScopeK where
  CloseAction ::
    (m ~ Lang ctx effs, s # a .| ctx) =>
    Name s ->
    RelationTerm ctx a ->
    Effect (Closure 'ActionClosure) m ()

data instance Effect (Closure 'InitialClosure) :: ScopeK where
  CloseInitial ::
    (m ~ Lang ctx effs, s # a .| ctx) =>
    Name s ->
    Lang ctx '[NonDet] a ->
    Effect (Closure 'InitialClosure) m ()
