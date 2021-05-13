-- | Temporal operators.
--
-- @since 0.1.0.0
module Language.Spectacle.Syntax.Modal
  ( -- * Labels
    Modal (Modal),
    Effect (Always, UpUntil),

    -- * Syntax
    always,
    eventually,
    upUntil,

    -- * Interpreters
    introducePrimes,
    applyModality,
  )
where

import Data.Functor.Loom (hoist, runLoom, (~>~))
import Language.Spectacle.Lang
  ( Lang (Op, Pure, Scoped),
    Member (injectS, projectS),
    Members,
    Op (OHere, OThere),
    Scoped (SHere, SThere),
    scope,
  )
import Language.Spectacle.Syntax.Logic.Internal (Logic)
import Language.Spectacle.Syntax.Modal.Internal (Effect (Always, UpUntil), Modal (Modal))
import Language.Spectacle.Syntax.Plain (Effect (PlainVar))
import Language.Spectacle.Syntax.Plain.Internal (Plain)
import Language.Spectacle.Syntax.Prime (Effect (PrimeVar), Prime)

-- ---------------------------------------------------------------------------------------------------------------------

-- | The modal operator 'always' qualifies a formula @p@ such that for all actions taken in a specification, the formula
-- @p@ must be true.
--
-- [Valid]
--
-- @
-- ... ───── p ───── p ───── p ───── p ───── p ───── ...
-- @
--
-- [Invalid]
--
-- @
-- ... ───── p ───── p ───── p ───── ○ ───── p ───── ...
-- @
--
-- @since 0.1.0.0
always :: Member Modal effs => Lang ctx effs Bool -> Lang ctx effs Bool
always m = scope (Always m)
{-# INLINE always #-}

-- | The modal operator 'eventually' qualifies a formula @p@ such that @p@ must be true /now/, or some time in the
-- future. 'eventually' is equivalent to:
--
-- @
-- 'eventually' p = 'upUntil' ('pure' 'True') p
-- @
--
-- [Valid]
--
-- @
-- ... ───── ○ ───── ○ ───── ○ ───── p ───── ○ ───── ...
--
-- ... ───── ○ ───── ○ ───── p ───── ○ ───── p ───── ...
--
-- ... ───── p ───── p ───── p ───── p ───── p ───── ...
-- @
--
-- [Invalid]
--
-- @
-- ... ───── ○ ───── ○ ───── ○ ───── ○ ───── ○ ───── ...
-- @
--
-- @since 0.1.0.0
eventually :: Member Modal effs => Lang ctx effs Bool -> Lang ctx effs Bool
eventually = upUntil (pure True)

-- | The modal operator 'upUntil' (strong) says that for some formula @p `upUntil` q@, @p@ must be true up until the
-- point where @q@ is true, and that @q@ /must/ be true some time in the future.
--
-- [Valid]
--
-- @
-- ... ───── p ───── q ───── q ───── ○ ───── ○ ───── ...
--
-- ... ─── p ∧ q ─── ○ ───── ○ ───── ○ ───── ○ ───── ...
--
-- ... ───── p ───── q ───── ○ ───── p ───── q ───── ...
-- @
--
-- [Invalid]
--
-- @
-- ... ───── p ───── p ───── p ───── ○ ───── q ───── ...
--
-- ... ───── p ───── p ───── p ───── p ───── p ───── ...
--
-- ... ───── ○ ───── ○ ───── ○ ───── ○ ───── ○ ───── ...
-- @
--
-- @since 0.1.0.0
upUntil :: Member Modal effs => Lang ctx effs Bool -> Lang ctx effs Bool -> Lang ctx effs Bool
upUntil m n = scope (UpUntil m n)

-- | Introduces a prime effect underneath the 'Logic' and 'Modal' effects in a temporal formula. The prime effect is
-- used to replace the plain variable usage on the right-hand side of 'upUntil' with prime variables. This is so we can
-- substitute the new values obtained by running an action to check if a 'upUntil' is satisfied.
--
-- 'Prime' is introduced only when needed to avoid giving users access to the 'prime' syntax in temporal formula.
--
-- @since 0.1.0.0
introducePrimes :: Lang ctx (Modal ': Logic ': effs) a -> Lang ctx (Modal ': Logic ': Prime ': effs) a
introducePrimes = \case
  Pure x -> pure x
  Op op k -> Op (extendOp op) (introducePrimes . k)
  Scoped scoped loom -> Scoped (extendS scoped) (loom ~>~ hoist introducePrimes)
  where
    extendOp :: Op (Modal ': Logic ': effs) a -> Op (Modal ': Logic ': Prime ': effs) a
    extendOp = \case
      OHere op -> OHere op
      OThere (OHere op) -> OThere (OHere op)
      OThere (OThere op) -> OThere (OThere (OThere op))

    extendS :: Scoped (Modal ': Logic ': effs) m a -> Scoped (Modal ': Logic ': Prime ': effs) m a
    extendS = \case
      SHere scoped -> SHere scoped
      SThere (SHere scoped) -> SThere (SHere scoped)
      SThere (SThere scoped) -> SThere (SThere (SThere scoped))
{-# INLINE introducePrimes #-}

-- | 'applyModality' is run after 'introducePrimes'. It replaces all occurances of 'plain' enclosed by the right-hand
-- side of 'upUntil' with 'prime'.
--
-- @since 0.1.0.0
applyModality :: Members '[Modal, Logic, Plain, Prime] effs => Lang ctx effs Bool -> Lang ctx effs Bool
applyModality = \case
  Pure x -> pure x
  Op op k -> Op op (applyModality . k)
  Scoped scoped loom -> case projectS scoped of
    Nothing -> Scoped scoped loomModal
    Just (Always expr) -> do
      let expr' = runLoom loomModal expr
      scope (Always expr')
    Just (UpUntil lhs rhs) -> do
      let lhs' = runLoom loomModal lhs
          rhs' = runLoom (loomModal ~>~ hoist replacePrimes) rhs
      scope (UpUntil lhs' rhs')
    where
      loomModal = loom ~>~ hoist applyModality
  where
    replacePrimes :: Members '[Plain, Prime] effs => Lang ctx effs a -> Lang ctx effs a
    replacePrimes = \case
      Pure x -> pure x
      Op op k -> Op op (replacePrimes . k)
      Scoped scoped loom -> case projectS scoped of
        Nothing -> Scoped scoped loomPrimed
        Just (PlainVar name) -> Scoped (injectS (PrimeVar name)) loomPrimed
        where
          loomPrimed = loom ~>~ hoist replacePrimes
