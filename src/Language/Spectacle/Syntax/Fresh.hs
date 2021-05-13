-- | Fresh 'Int' stream effect.
--
-- @since 0.1.0.0
module Language.Spectacle.Syntax.Fresh
  ( -- * Labels
    Fresh (Fresh),
    Effect (FreshH),

    -- * Syntax
    fresh,

    -- * Interpreters
    runFresh,
  )
where

import Data.Coerce (coerce)
import Data.Void (absurd)

import Data.Functor.Loom (weave, (~>~))
import Language.Spectacle.Lang (Effect, Lang (Op, Pure, Scoped), Member, decomposeOp, decomposeS, send)
import Language.Spectacle.Syntax.Fresh.Internal (Effect (FreshH), Fresh (Fresh))

-- ---------------------------------------------------------------------------------------------------------------------

-- | Obtain a fresh 'Int' from.
--
-- @since 0.1.0.0
fresh :: Member Fresh effs => Lang ctx effs Int
fresh = send Fresh
{-# INLINE fresh #-}

-- | Discharge a 'Fresh' effect starting with a given 'Int'.
--
-- @since 0.1.0.0
runFresh :: Int -> Lang ctx (Fresh ': effs) a -> Lang ctx effs (Int, a)
runFresh n = \case
  Pure x -> pure (n, x)
  Op op k -> case decomposeOp op of
    Left other -> Op other (k' n)
    Right Fresh -> k' (n + 1) n
    where
      k' n' = runFresh n' . k
  Scoped scoped loom -> case decomposeS scoped of
    Left other -> Scoped other loomFresh
    Right bottom -> absurd (coerce bottom)
    where
      loomFresh = loom ~>~ weave (n, ()) (uncurry runFresh)
