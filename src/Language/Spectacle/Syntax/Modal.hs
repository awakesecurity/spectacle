{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE TupleSections #-}

module Language.Spectacle.Syntax.Modal
  ( Modal (Modal),
    Effect (Always, UpUntil),
    always,
    eventually,
    upUntil,
    testModalResult1,
  )
where

import Control.Monad
import Data.Coerce
import Data.Void

import Data.Functor.Loom
import Language.Spectacle.Lang
import Language.Spectacle.Syntax.Modal.Internal
import Language.Spectacle.Syntax.Modal.Quoted
import Language.Spectacle.Syntax.Logic
import Language.Spectacle.Syntax.NonDet
import Language.Spectacle.Syntax.Error
import Language.Spectacle.Exception.RuntimeException
import Data.Function

-- -------------------------------------------------------------------------------------------------

always :: Member Modal effs => Lang ctx effs Bool -> Lang ctx effs Bool
always m = scope (Always m)

eventually :: Member Modal effs => Lang ctx effs Bool -> Lang ctx effs Bool
eventually = upUntil (pure True)

upUntil :: Member Modal effs => Lang ctx effs Bool -> Lang ctx effs Bool -> Lang ctx effs Bool
upUntil m n = scope (UpUntil m n)


runModal ::
  Lang ctx (Modal ': Logic ': effs) a ->
  Quoted ctx effs (Modal ': Logic ': effs) a
runModal modal = quoting \cont -> case modal of
  Pure x -> cont (ConstQ x)
  Yield (Op op) k -> case decomposeOp op of
    Left op' -> case decomposeOp op' of
      Left other -> Yield (Op other) (lowerQuoted . runModal . k) >>= cont
      Right bottom -> absurd (coerce bottom)
    Right bottom -> absurd (coerce bottom)
  Yield union@(Scoped scoped loom) k -> case decomposeS scoped of
    Left scoped' -> case decomposeS scoped' of
      Left other -> Yield (Scoped other loom') k' >>= cont
      Right (Forall xs p) -> do
        metas <- traverse ((runLoom loom' >=> k') . p) xs
        cont (ModalQ (MetaForall metas) union k)
      Right (Exists xs p) -> do
        metas <- traverse ((runLoom loom' >=> k') . p) xs
        cont (ModalQ (MetaExists metas) union k)
      Right (Complement m) -> do
        meta <- runLoom loom' m >>= k'
        cont (ModalQ (MetaComplement meta) union k)
      Right (Conjunct m n) -> do
        lhs <- runLoom loom' m >>= k'
        rhs <- runLoom loom' n >>= k'
        cont (ModalQ (MetaConjunct lhs rhs) union k)
      Right (Disjunct m n) -> do
        lhs <- runLoom loom' m >>= k'
        rhs <- runLoom loom' n >>= k'
        cont (ModalQ (MetaDisjunct lhs rhs) union k)
    Right (Always m) -> do
      meta <- runLoom loom' m >>= k'
      cont (ModalQ (MetaAlways meta) union k)
    Right (UpUntil m n) -> do
      lhs <- runLoom loom' m >>= k'
      rhs <- runLoom loom' n >>= k'
      cont (ModalQ (MetaUpUntil lhs rhs) union k)
    where
      loom' = weave (ConstQ ()) adjuncted loom
        where
          adjuncted ::
            ModalQ ctx (Modal ': Logic ': effs) (Lang ctx (Modal ': Logic ': effs) x) ->
            Lang ctx effs (ModalQ ctx (Modal ': Logic ': effs) x)
          adjuncted = lowerQuoted . runModal . join . reifyQ

      k' = lowerQuoted . runModal . (reifyQ >=> k)
