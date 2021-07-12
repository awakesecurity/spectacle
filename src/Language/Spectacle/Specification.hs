module Language.Spectacle.Specification
  ( Specification
      ( Specification,
        initialAction,
        nextAction,
        temporalFormula,
        terminationFormula,
        fairnessConstraint
      ),
  )
where

import Language.Spectacle.AST (Action, Initial, Invariant, Terminate)
import Language.Spectacle.Checker.Fairness (Fairness)

-- ---------------------------------------------------------------------------------------------------------------------

-- | A complete spectacle specification.
--
-- @since 0.1.0.0
data Specification ctx = Specification
  { initialAction :: Initial ctx ()
  , nextAction :: Action ctx Bool
  , temporalFormula :: Invariant ctx Bool
  , terminationFormula :: Maybe (Terminate ctx Bool)
  , fairnessConstraint :: Fairness
  }
