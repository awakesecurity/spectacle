{-# LANGUAGE TypeFamilies #-}

module Language.Spectacle.Syntax.Quantify.Internal
  ( Quantify (Quantify),
    Effect (Forall, Exists),
  )
where

import Data.Void (Void)

import Language.Spectacle.Exception.RuntimeException (RuntimeException)
import Language.Spectacle.Lang (Effect, EffectK, Lang, Members, ScopeK)
import Language.Spectacle.Syntax.Error.Internal (Error)
import Language.Spectacle.Syntax.NonDet.Internal (NonDet)

-- -------------------------------------------------------------------------------------------------

newtype Quantify :: EffectK where
  Quantify :: Void -> Quantify a

data instance Effect Quantify :: ScopeK where
  Forall ::
    (Members '[Error RuntimeException, NonDet] effs, m ~ Lang ctx effs) =>
    [a] ->
    (a -> m Bool) ->
    Effect Quantify m a
  Exists ::
    (Members '[Error RuntimeException, NonDet] effs, m ~ Lang ctx effs) =>
    [a] ->
    (a -> m Bool) ->
    Effect Quantify m a
