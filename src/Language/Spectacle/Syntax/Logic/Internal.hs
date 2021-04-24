{-# LANGUAGE TypeFamilies #-}

module Language.Spectacle.Syntax.Logic.Internal
  ( Logic (Logic),
    Effect (Forall, Exists, Complement, Conjunct, Disjunct),
  )
where

import Data.Void (Void)

import Language.Spectacle.Exception.RuntimeException (RuntimeException)
import Language.Spectacle.Lang (Effect, EffectK, Lang, Members, ScopeK)
import Language.Spectacle.Syntax.Error.Internal (Error)
import Language.Spectacle.Syntax.NonDet.Internal (NonDet)

-- -------------------------------------------------------------------------------------------------

newtype Logic :: EffectK where
  Logic :: Void -> Logic a

data instance Effect Logic :: ScopeK where
  Forall ::
    (Members '[Logic, Error RuntimeException, NonDet] effs, m ~ Lang ctx effs) =>
    [a] ->
    (a -> m Bool) ->
    Effect Logic m a
  Exists ::
    (Members '[Logic, Error RuntimeException, NonDet] effs, m ~ Lang ctx effs) =>
    [a] ->
    (a -> m Bool) ->
    Effect Logic m a
  Complement ::
    (m ~ Lang ctx effs, Members Logic effs) =>
    m a ->
    Effect Logic m a
  Conjunct ::
    (m ~ Lang ctx effs, Members Logic effs) =>
    m Bool ->
    m Bool ->
    Effect Logic m Bool
  Disjunct ::
    (m ~ Lang ctx effs, Members Logic effs) =>
    m Bool ->
    m Bool ->
    Effect Logic m Bool
