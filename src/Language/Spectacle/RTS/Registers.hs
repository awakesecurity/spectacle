{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE UndecidableInstances #-}

-- | Runtime state used to implement call-by-need evaluation of closures and variable substitution.
--
-- @since 0.1.0.0
module Language.Spectacle.RTS.Registers
  ( RuntimeState (RuntimeState, plains, primes, callStack, newValues),
    emptyRuntimeState,
    Registers (Registers, unRegisters),
    emptyRegisters,
    getRegister,
    setRegister,
    setThunk,
    StateFun (StateFun, getStateFun),
    type StateFunSyntax,
    Thunk (Thunk, Evaluated, Unchanged),
  )
where

import Data.Kind (Type)

import Data.Ascript (type (#))
import Data.Context (Context, type (:<))
import Data.Type.Rec (HasSel (getRecT, setRecT), Name, Rec, RecT, fieldMap, type (.|))
import Language.Spectacle.Exception.RuntimeException (RuntimeException)
import Language.Spectacle.Lang (EffectK, Lang, scope)
import Language.Spectacle.Syntax.Error.Internal (Error)
import Language.Spectacle.Syntax.NonDet.Internal (NonDet)
import Language.Spectacle.Syntax.Plain.Internal (Effect (PlainVar), Plain, PlainIntro (plainIntro))
import Language.Spectacle.Syntax.Prime.Internal (Effect (PrimeVar), Prime, PrimeIntro (primeIntro))

-- ---------------------------------------------------------------------------------------------------------------------

newtype StateFun :: Context -> Type -> Type where
  StateFun :: {getStateFun :: Lang ctxt StateFunSyntax a} -> StateFun ctxt a
  deriving (Functor, Applicative, Monad)

instance s # a .| ctxt => PlainIntro (StateFun ctxt) s a where
  plainIntro name = StateFun (scope (PlainVar name))
  {-# INLINE plainIntro #-}

instance s # a .| ctxt => PrimeIntro (StateFun ctxt) s a where
  primeIntro name = StateFun (scope (PrimeVar name))
  {-# INLINE primeIntro #-}

type StateFunSyntax :: [EffectK]
type StateFunSyntax = '[Prime, Plain, NonDet, Error RuntimeException]

-- | Internal state used by variable substitution and variable relations.
--
-- * 'plains' is a record relating plain variables in @ctx@ to values in the previous frame of time.
-- * 'primes' is a record relating primed or "time" variables in @ctx@ to their values in the next
-- frame of time.
-- * 'callStack' tracks the substitution of primed variables which is used guard against a cyclic
-- variable relations.
--
-- @since 0.1.0.0
data RuntimeState ctx = RuntimeState
  { plains :: Rec ctx
  , primes :: Registers ctx
  , callStack :: [String]
  , newValues :: Rec ctx
  }

deriving instance (Show (Rec ctx), Show (Registers ctx)) => Show (RuntimeState ctx)

emptyRuntimeState :: Rec ctx -> RuntimeState ctx
emptyRuntimeState r =
  RuntimeState
    { plains = r
    , primes = emptyRegisters r
    , callStack = mempty
    , newValues = r
    }

-- | A record of 'Thunk's.
--
-- @since 0.1.0.0
newtype Registers ctxt = Registers
  {unRegisters :: RecT (Thunk ctxt) ctxt}

deriving instance Show (RecT (Thunk ctxt) ctxt) => Show (Registers ctxt)

-- | Construct a 'Registers' of unrelated primed variables.
--
-- @since 0.1.0.0
emptyRegisters :: Rec ctxt -> Registers ctxt
emptyRegisters = Registers . fieldMap (const Unchanged)

-- | Retrieves the value of the variable named @s@ in 'Registers' as a 'Thunk'.
--
-- @since 0.1.0.0
getRegister :: s # a .| ctxt => Name s -> Registers ctxt -> Thunk ctxt a
getRegister n (Registers rs) = getRecT n rs

-- | Sets the value of the variable named @s@ to the result given by evaluating a 'Thunk'.
--
-- @since 0.1.0.0
setRegister :: s # a .| ctxt => Name s -> a -> Registers ctxt -> Registers ctxt
setRegister n x (Registers rs) = Registers (setRecT n (Evaluated x) rs)

-- | Sets the value of the variable named @s@ in 'Registers' to an unevaluated expression.
--
-- @since 0.1.0.0
setThunk :: s # a .| ctxt => Name s -> StateFun ctxt a -> Registers ctxt -> Registers ctxt
setThunk n m (Registers rs) = Registers (setRecT n (Thunk m) rs)

-- | A 'Thunk' is the state of a primed variable in @ctx@.
--
-- * 'Thunk' is an unevaluated expression on the right-hand side of a closure.
-- * 'Evaluated' result of evaluating a thunk in current world.
-- * Any primed variable is implicitly 'Unchanged' if it has not been related in the current world.
--
-- @since 0.1.0.0
data Thunk ctxt a
  = Thunk (StateFun ctxt a)
  | Evaluated a
  | Unchanged

-- | @since 0.1.0.0
instance Show a => Show (Thunk (s # a :< ctx) a) where
  show (Thunk _) = "<<thunk>>"
  show (Evaluated x) = show x
  show Unchanged = "<<unchanged>>"
