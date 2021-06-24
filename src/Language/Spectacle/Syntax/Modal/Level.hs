module Language.Spectacle.Syntax.Modal.Level
  ( -- * Expression Levels
    ExprLevel (L1, L2, L3, L4),
  )
where

-- ---------------------------------------------------------------------------------------------------------------------

-- | Enumeration of expression levels in TLA.
--
-- * 'L1' - constant level expressions
--
-- * 'L2' - state level expressions
--
-- * 'L3' - action level expressions
--
-- * 'L4' - temporal level expressions
--
-- @since 0.1.0.0
data ExprLevel = L1 | L2 | L3 | L4
  deriving (Enum, Eq, Ord, Show)
