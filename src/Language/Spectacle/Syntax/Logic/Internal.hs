{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TypeFamilies #-}

module Language.Spectacle.Syntax.Logic.Internal
  ( Logic (Logic),
    Effect (Forall, Exists, Complement, Conjunct, Disjunct),
  )
where

import Data.Void (Void)

import Language.Spectacle.Exception.RuntimeException (RuntimeException)
import Language.Spectacle.Lang (Effect, EffectK, Lang, Member, Members, ScopeK)
import Language.Spectacle.Syntax.Error.Internal (Error)
import Language.Spectacle.Syntax.NonDet.Internal (NonDet)

-- -------------------------------------------------------------------------------------------------

newtype Logic :: EffectK where
  Logic :: Void -> Logic a

data instance Effect Logic :: ScopeK where
  Forall ::
    (m ~ Lang ctx effs, Members '[Logic, NonDet, Error RuntimeException] effs) =>
    [a] ->
    (a -> m Bool) ->
    Effect Logic m Bool
  Exists ::
    (m ~ Lang ctx effs, Members '[Logic, Error RuntimeException, NonDet] effs) =>
    [a] ->
    (a -> m Bool) ->
    Effect Logic m Bool
  Complement ::
    (m ~ Lang ctx effs, Member Logic effs) =>
    m Bool ->
    Effect Logic m Bool
  Conjunct ::
    (m ~ Lang ctx effs, Members '[Logic, NonDet] effs) =>
    m Bool ->
    m Bool ->
    Effect Logic m Bool
  Disjunct ::
    (m ~ Lang ctx effs, Members '[Logic, NonDet] effs) =>
    m Bool ->
    m Bool ->
    Effect Logic m Bool
