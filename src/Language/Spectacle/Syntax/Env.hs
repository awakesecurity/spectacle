-- |
-- Module      :  Language.Spectacle.Syntax.Env
-- Copyright   :  (c) Arista Networks, 2022-2023
-- License     :  Apache License 2.0, see LICENSE
--
-- Stability   :  stable
-- Portability :  non-portable (GHC extensions)
--
-- TODO: docs
--
-- @since 1.0.0
module Language.Spectacle.Syntax.Env
  ( Env (Env),
    Effect (Get, Put),
    get,
    gets,
    put,
    modify,
    runEnv,
  )
where

import Data.Coerce (coerce)
import Data.Void (absurd)

import Data.Functor.Loom (hoist, runLoom, (~>~))
import Language.Spectacle.Lang (Effect, Lang (Op, Pure, Scoped), Member, decomposeOp, decomposeS, scope)
import Language.Spectacle.RTS.Registers (RuntimeState)
import Language.Spectacle.Syntax.Env.Internal (Effect (Get, Put), Env (Env))

-- ---------------------------------------------------------------------------------------------------------------------

get :: Member Env effs => Lang ctx effs (RuntimeState ctx)
get = scope Get
{-# INLINE get #-}

gets :: Member Env effs => (RuntimeState ctx -> s) -> Lang ctx effs s
gets f = fmap f get
{-# INLINE gets #-}

put :: Member Env effs => RuntimeState ctx -> Lang ctx effs ()
put x = scope (Put x)
{-# INLINE put #-}

modify :: Member Env effs => (RuntimeState ctx -> RuntimeState ctx) -> Lang ctx effs ()
modify f = get >>= put . f
{-# INLINE modify #-}

runEnv :: RuntimeState ctx -> Lang ctx (Env ': effs) a -> Lang ctx effs (RuntimeState ctx, a)
runEnv st = \case
  Pure x -> pure (st, x)
  Op op k -> case decomposeOp op of
    Left other -> Op other (runEnv st . k)
    Right (Env b) -> absurd (coerce b)
  Scoped scoped loom -> case decomposeS scoped of
    Left other -> Scoped other (loom' st)
    Right eff
      | Get <- eff -> runLoom (loom' st) (pure st)
      | Put st' <- eff -> runLoom (loom' st') (pure ())
    where
      loom' st' = loom ~>~ hoist (runEnv st')
