-- |
-- Module      :  Language.Spectacle.Syntax.Enabled
-- Copyright   :  (c) Arista Networks, 2022-2023
-- License     :  Apache License 2.0, see LICENSE
--
-- Stability   :  stable
-- Portability :  non-portable (GHC extensions)
--
-- TODO: docs
--
-- @since 1.0.0
module Language.Spectacle.Syntax.Enabled
  ( -- * Labels
    Enabled (Enabled),
    Effect (EnabledS),
    enabled,

    -- ** Interpreters
    runEnabled,
  )
where

import Data.Void (absurd)

import Data.Functor.Loom (hoist, (~>~))
import Language.Spectacle.Lang
  ( Effect,
    Lang (Op, Pure, Scoped),
    Member,
    decomposeOp,
    decomposeS,
    send,
  )
import Language.Spectacle.Syntax.Enabled.Internal (Effect (EnabledS), Enabled (Enabled))

-- ---------------------------------------------------------------------------------------------------------------------

enabled :: Member Enabled effs => Lang ctx effs Bool
enabled = send Enabled
{-# INLINE enabled #-}

runEnabled :: Bool -> Lang ctx (Enabled : effs) a -> Lang ctx effs a
runEnabled isEnabled = \case
  Pure x -> pure x
  Op op k -> case decomposeOp op of
    Left other -> Op other (runEnabled isEnabled . k)
    Right Enabled -> runEnabled isEnabled (k isEnabled)
  Scoped scoped loom -> case decomposeS scoped of
    Left other -> Scoped other (loom ~>~ hoist (runEnabled isEnabled))
    Right (EnabledS bottom) -> absurd bottom
{-# INLINE runEnabled #-}
