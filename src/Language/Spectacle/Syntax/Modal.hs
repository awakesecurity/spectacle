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
  )
where

import Language.Spectacle.Lang (Effect, Lang, Members, scope)
import Language.Spectacle.Syntax.Fresh (Fresh, fresh)
import Language.Spectacle.Syntax.Modal.Internal (Effect (Always, UpUntil), Modal (Modal))

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
always :: Members '[Modal, Fresh] effs => Lang ctx effs Bool -> Lang ctx effs Bool
always m = do
  name <- fresh
  scope (Always name m)
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
eventually :: Members '[Modal, Fresh] effs => Lang ctx effs Bool -> Lang ctx effs Bool
eventually = upUntil (pure True)
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
upUntil :: Members '[Modal, Fresh] effs => Lang ctx effs Bool -> Lang ctx effs Bool -> Lang ctx effs Bool
upUntil m n = do
  name <- fresh
  scope (UpUntil name m n)
{-# INLINE upUntil #-}
