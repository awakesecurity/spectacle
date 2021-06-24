{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE UndecidableInstances #-}

module Language.Spectacle.Exception.ModelCheckerException
  ( ModelCheckerException
      ( NoInitialStates,
        FormulaException,
        FormulaInfiniteStutter,
        TerminationException,
        ImpassException
      ),
    ImpassException
      ( ImpassNoTermination,
        ImpassFailedTerminate,
        ImpassInfiniteStutter
      ),
    FormulaException
      ( UnsatisfiedInvariant,
        PropertyViolated,
        CyclicException
      ),
    TerminationException
      ( UnsatisfiedImplication
      ),
    CyclicCheckException
      ( CyclicViolation,
        CyclicNoInfo
      ),
  )
where

import Control.Exception (Exception)
import Type.Reflection (Typeable)

import Data.Type.Rec (Rec)
import Language.Spectacle.Spec.Base (Modality)

-- ---------------------------------------------------------------------------------------------------------------------

data ModelCheckerException where
  NoInitialStates :: ModelCheckerException
  FormulaException :: FormulaException -> ModelCheckerException
  FormulaInfiniteStutter :: FormulaException -> ModelCheckerException
  TerminationException :: TerminationException -> ModelCheckerException
  ImpassException :: ImpassException -> ModelCheckerException
  deriving stock (Typeable)
  deriving anyclass (Exception)

deriving instance Show ModelCheckerException

data ImpassException where
  ImpassNoTermination :: Show (Rec ctx) => Rec ctx -> ImpassException
  ImpassFailedTerminate :: Show (Rec ctx) => Rec ctx -> ImpassException
  ImpassInfiniteStutter :: Show (Rec ctx) => Rec ctx -> ImpassException
  deriving stock (Typeable)
  deriving anyclass (Exception)

deriving instance Show ImpassException

data FormulaException where
  UnsatisfiedInvariant :: Show (Rec ctx) => Rec ctx -> Rec ctx -> FormulaException
  PropertyViolated :: Show (Rec ctx) => Int -> Modality -> Rec ctx -> FormulaException
  CyclicException :: CyclicCheckException -> FormulaException
  deriving (Typeable)
  deriving anyclass (Exception)

deriving instance Show FormulaException

data TerminationException where
  UnsatisfiedImplication :: Show (Rec ctx) => Int -> Modality -> Rec ctx -> TerminationException
  deriving (Typeable)
  deriving anyclass (Exception)

deriving instance Show TerminationException

data CyclicCheckException where
  CyclicViolation :: Show (Rec ctx) => Int -> Modality -> Rec ctx -> Rec ctx -> CyclicCheckException
  CyclicNoInfo :: Show (Rec ctx) => Int -> Modality -> Rec ctx -> CyclicCheckException
  deriving stock (Typeable)
  deriving anyclass (Exception)

deriving instance Show CyclicCheckException
