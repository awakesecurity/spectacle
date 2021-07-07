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

    -- * Syntactic Levels
    ExprLevel (L1, L2, L3, L4),

    -- * Preterms
    Preterm
      ( PreConst,
        PreConjunct,
        PreDisjunct,
        PreComplement,
        PreAlways,
        PreUpUntil
      ),
    pretermFromModal,
    pretermToModal,
    normalForm,
  )
where

import Data.Function ((&))
import Data.Void (absurd)
import GHC.Stack
    ( CallStack, SrcLoc, getCallStack, callStack, HasCallStack )

import Data.Functor.Loom (hoist, runLoom, (~>~))
import Language.Spectacle.Lang
  ( Effect,
    Lang (Op, Pure, Scoped),
    Member,
    Members,
    decomposeOp,
    decomposeS,
    scope,
    weaken,
  )
import Language.Spectacle.Syntax.Logic
  ( Effect (Complement, Conjunct, Disjunct),
    Logic (Logic),
    complement,
    conjunct,
    disjunct,
  )
import Language.Spectacle.Syntax.Modal.Internal
  ( Effect (Always, UpUntil),
    Modal (Modal),
  )
import Language.Spectacle.Syntax.Modal.Level (ExprLevel (L1, L2, L3, L4))
import Language.Spectacle.Syntax.Modal.Preterm
  ( Preterm
      ( PreAlways,
        PreComplement,
        PreConjunct,
        PreConst,
        PreDisjunct,
        PreEventually,
        PreUpUntil
      ),
    normalForm,
  )

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
always :: (HasCallStack, Member Modal effs) => Lang ctx effs Bool -> Lang ctx effs Bool
always m = scope (Always (getSrcLocInfo callStack) m)
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
eventually :: (HasCallStack, Member Modal effs) => Lang ctx effs Bool -> Lang ctx effs Bool
eventually m = scope (UpUntil (getSrcLocInfo callStack) (pure True) m)
{-# INLINE eventually #-}

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
upUntil :: (HasCallStack, Member Modal effs) => Lang ctx effs Bool -> Lang ctx effs Bool -> Lang ctx effs Bool
upUntil m n = scope (UpUntil (getSrcLocInfo callStack) m n)
{-# INLINE upUntil #-}

-- | Discharges the 'Modal' and 'Logic' effects to an equivalent preterm expression.
--
-- @since 0.1.0.0
pretermFromModal :: Lang ctx (Modal ': Logic ': effs) a -> Lang ctx effs (Preterm a)
pretermFromModal = \case
  Pure x -> pure (PreConst x)
  Op op k -> case decomposeOp op of
    Left op' -> case decomposeOp op' of
      Left other -> Op other (pretermFromModal . k)
      Right (Logic b) -> absurd b
    Right (Modal b) -> absurd b
  Scoped scoped loom -> case decomposeS scoped of
    Left scoped' -> case decomposeS scoped' of
      Left other -> Scoped other loomReify
      Right eff
        | Conjunct lhs rhs <- eff -> do
          lhs' <- runLoom loomReify lhs
          rhs' <- runLoom loomReify rhs
          return (PreConjunct lhs' rhs')
        | Disjunct lhs rhs <- eff -> do
          lhs' <- runLoom loomReify lhs
          rhs' <- runLoom loomReify rhs
          return (PreDisjunct lhs' rhs')
        | Complement expr <- eff -> do
          expr' <- runLoom loomReify expr
          return (PreComplement expr')
    Right eff
      | Always loc expr <- eff -> do
        expr' <- runLoom loomReify expr
        return (PreAlways loc expr')
      | UpUntil loc Pure {} expr <- eff -> do
        expr' <- runLoom loomReify expr
        return (PreEventually loc expr')
      | UpUntil loc lhs rhs <- eff -> do
        lhs' <- runLoom loomReify lhs
        rhs' <- runLoom loomReify rhs
        return (PreUpUntil loc lhs' rhs')
    where
      loomReify = loom ~>~ hoist pretermFromModal
{-# INLINE pretermFromModal #-}

-- | Sends a preterm expressin to the corresponding effects.
--
-- @since 0.1.0.0
pretermToModal :: Lang ctx effs (Preterm Bool) -> Lang ctx (Modal ': Logic ': effs) Bool
pretermToModal terms =
  terms
    & weaken
    & weaken
    & (>>= sendModal)
  where
    sendModal :: Members '[Modal, Logic] effs => Preterm Bool -> Lang ctx effs Bool
    sendModal = \case
      PreConst x -> pure x
      PreConjunct e1 e2 -> conjunct (sendModal e1) (sendModal e2)
      PreDisjunct e1 e2 -> disjunct (sendModal e1) (sendModal e2)
      PreComplement e -> complement (sendModal e)
      PreUpUntil loc e1 e2 -> scope (UpUntil loc (sendModal e1) (sendModal e2))
      PreEventually loc e -> scope (UpUntil loc (pure True) (sendModal e))
      PreAlways loc e -> scope (Always loc (sendModal e))
{-# INLINE pretermToModal #-}

getSrcLocInfo :: CallStack -> Maybe SrcLoc
getSrcLocInfo stack = case getCallStack stack of 
  [] -> Nothing
  x : _ -> Just (snd x)
{-# INLINE getSrcLocInfo #-}
