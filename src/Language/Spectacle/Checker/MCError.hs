{-# LANGUAGE UndecidableInstances #-}

-- | Model checker error productions.
--
-- @since 0.1.0.0
module Language.Spectacle.Checker.MCError
  ( MCError
      ( MCNoInitialStatesError,
        MCActionError,
        MCImpasseError,
        MCEventuallyError,
        MCStutterError,
        MCFormulaError,
        MCFormulaRuntimeError,
        MCStrongLivenessError,
        MCInternalError
      ),
    StutterKind
      ( InfiniteStutterK,
        FiniteStutterK
      ),
    PropertyKind
      ( AlwaysPropK,
        EventuallyPropK,
        UpUntilPropK,
        StaysAsPropK,
        InfinitelyOftenPropK
      ),
    InternalErrorKind
      ( EmptyDisjunctQueueK
      ),
  )
where

import Data.Set (Set)
import GHC.Stack (SrcLoc)

import Data.Type.Rec (Rec)
import Data.World (World)
import Language.Spectacle.Checker.Fingerprint (Fingerprint)
import Language.Spectacle.Checker.Step ( Step )
import Language.Spectacle.Exception.RuntimeException (RuntimeException)

-- ---------------------------------------------------------------------------------------------------------------------

-- | 'MCError' is a sum of the kinds of errors the model checker can emit when it fails.
--
-- @since 0.1.0.0
data MCError ctxt
  = -- | 'MCNoInitialStatesError' is reported when the initial action is evaluated to an empty set.
    MCNoInitialStatesError
  | -- | 'MCActionError' is an error occuring in the expansion of a model's next-state relation. It gives the world the
    -- action cannot be run from along with the 'RuntimeException' raised when evaluating the next-state relation.
    MCActionError
      (World ctxt)
      RuntimeException
  | -- | 'MCImpasseError' is emitted when the next-state relation successfully evaluates but produces an empty set. The
    -- 'World' which is not related to any new states is given here.
    MCImpasseError
      (World ctxt)
  | -- | 'MCEventuallyError' is emitted when the an A-step qualified with @Eventually A@ is never taken. The initial
    -- world, final world, unsatisfied step names, and depth of the final world are provided.
    MCEventuallyError
      Fingerprint
      Fingerprint
      (Set String)
      Int
  | -- | 'MCStutterError' is an elaboration on 'MCFormulaError' that occurs specifically for when a model's temporal
    -- formula is not satisfied for a stuttering-step.
    --
    -- Along with the step that violates the model's temporal formula, we give the source location of the violated
    -- property and the kind of modality the violated formula was qualified by; the kind of stuttering is also given
    -- which is 'InfiniteStutterK' in the case of
    --
    -- @w -> w -> ... -> w@
    --
    -- Or 'FiniteStutterK' in the case of
    --
    -- @w_1 -> w_1 -> w_2@
    MCStutterError
      (Step ctxt)
      (Maybe SrcLoc)
      PropertyKind
      StutterKind
  | -- | 'MCFormulaError' is an error for when a property of the temporal formula is violated. The step
    -- violating the property, the kind of modality for the property, and the source location of that property are given.
    MCFormulaError
      (Step ctxt)
      (Maybe SrcLoc)
      PropertyKind
  | -- | 'MCFormulaRuntimeError' is emitted when a 'RuntimeException' is raised while evaluating a model's temporal
    -- formula. Realistically the only exception that could show up here is a level-mismatch in the temporal formula.
    MCFormulaRuntimeError
      (Step ctxt)
      RuntimeException
  | -- | 'MCStrongLivenessError' is produced when a strong-fair process failed to satisfy a liveness property of the
    -- model. The source location of the property is given here.
    MCStrongLivenessError
      (Maybe SrcLoc)
  | -- | 'MCInternalError' is any internal model checker error. These /should/ be impossible but are exists so that any
    -- bugs in the model checker are obvious.
    MCInternalError InternalErrorKind

deriving instance Show (Rec ctxt) => Show (MCError ctxt)

-- | An an enumeration of the kinds of stuttering-steps that can occur while model checking.
--
-- @since 0.1.0.0
data StutterKind
  = InfiniteStutterK
  | FiniteStutterK
  deriving (Eq, Ord, Enum, Show)

-- | An enumeration of the kinds of property violations that can occur.
--
-- @since 0.1.0.0
data PropertyKind
  = AlwaysPropK
  | EventuallyPropK
  | UpUntilPropK
  | StaysAsPropK
  | InfinitelyOftenPropK
  deriving (Eq, Ord, Enum, Show)

-- | Internal model checker errors.
--
-- @since 0.1.0.0
data InternalErrorKind
  = EmptyDisjunctQueueK
  deriving (Eq, Ord, Enum, Show)
