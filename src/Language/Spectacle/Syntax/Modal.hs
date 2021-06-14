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

    -- * Leveled Syntax
    LTerm
      ( ValueL1,
        EmbedL1,
        ConjunctL2,
        DisjunctL2,
        ComplementL2,
        ImpliesL2,
        NotImpliesL2,
        AlwaysL3,
        EventuallyL3,
        UpUntilL3,
        InfinitelyOftenL3,
        StaysAsL3,
        ModalL4,
        ConjunctL4,
        DisjunctL4
      ),

    -- ** Syntactic Levels
    SyntaxLevel (fromPreterm),
    Level (L1, L2, L3, L4),
    levelMismatch,
    levelsAsNums,
    fromLevel,
    levelsOf,
    nameFromL3,

    -- * Preterms
    Preterm
      ( PreConst,
        PreConjunct,
        PreDisjunct,
        PreImplies,
        PreNotImplies,
        PreComplement,
        PreAlways,
        PreUpUntil
      ),
    pattern PreEventually,
    materialize,
    abstract,
    normalizePreterm,
    rewritePreterm,
  )
where

import Language.Spectacle.Lang (Effect, Lang, Members, scope)
import Language.Spectacle.Syntax.Fresh (Fresh, fresh)
import Language.Spectacle.Syntax.Modal.Graded
  ( LTerm
      ( AlwaysL3,
        ComplementL2,
        ConjunctL2,
        ConjunctL4,
        DisjunctL2,
        DisjunctL4,
        EmbedL1,
        EventuallyL3,
        ImpliesL2,
        InfinitelyOftenL3,
        ModalL4,
        NotImpliesL2,
        StaysAsL3,
        UpUntilL3,
        ValueL1
      ),
    Level (L1, L2, L3, L4),
    SyntaxLevel (fromPreterm),
    fromLevel,
    levelMismatch,
    levelsAsNums,
    levelsOf,
    nameFromL3,
  )
import Language.Spectacle.Syntax.Modal.Internal
  ( Effect (Always, UpUntil),
    Modal (Modal),
  )
import Language.Spectacle.Syntax.Modal.Preterm
  ( Preterm
      ( PreAlways,
        PreComplement,
        PreConjunct,
        PreConst,
        PreDisjunct,
        PreUpUntil
      ),
    abstract,
    materialize,
    normalizePreterm,
    rewritePreterm,
    pattern PreEventually,
    pattern PreImplies,
    pattern PreNotImplies,
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
