{-# LANGUAGE TypeFamilies #-}

module Language.Spectacle.Syntax.Env.Internal
  ( Env (Env),
    Effect (Get, Put),
  )
where

import Data.Void (Void)

import Language.Spectacle.Lang (Effect, EffectK)
import Language.Spectacle.RTS.Registers (RuntimeState)

-- ---------------------------------------------------------------------------------------------------------------------

newtype Env :: EffectK where
  Env :: Void -> Env a

data instance Effect Env m a where
  Get :: m ~ f ctx effs => Effect Env m (RuntimeState ctx)
  Put :: m ~ f ctx effs => RuntimeState ctx -> Effect Env m ()
