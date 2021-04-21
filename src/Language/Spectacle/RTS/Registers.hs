-- | Runtime state used to implement call-by-need evaluation of closures and variable substitution.
--
-- @since 0.1.0.0
module Language.Spectacle.RTS.Registers
  ( RuntimeState (RuntimeState, plains, primes, callStack),
    Registers (Registers, unRegisters),
    emptyRegisters,
    getRegister,
    setRegister,
    setThunk,
    Thunk (Thunk, Evaluated, Unchanged),
  )
where

import Data.Type.Rec (HasSel (getRecT, setRecT), Name, Rec, RecT, fieldMap, type (#), type (.|))
import Language.Spectacle.Exception.RuntimeException (RuntimeException)
import Language.Spectacle.Lang (Lang)
import Language.Spectacle.Syntax.Error.Internal (Error)
import Language.Spectacle.Syntax.NonDet.Internal (NonDet)
import Language.Spectacle.Syntax.Plain.Internal (Plain)
import Language.Spectacle.Syntax.Prime.Internal (Prime)

-- -------------------------------------------------------------------------------------------------

-- | Internal state used by variable substitution and variable relations.
--
-- * 'plains' is a record relating plain variables in @ctx@ to values in the previous frame of time.
-- * 'primes' is a record relating primed or "time" variables in @ctx@ to their values in the next
-- frame of time.
-- * 'callStack' tracks the substitution of primed variables to used guard against a cyclic
-- relation.
--
-- @since 0.1.0.0
data RuntimeState ctx = RuntimeState
  { plains :: Rec ctx
  , primes :: Registers ctx
  , callStack :: [String]
  }

-- | A record of 'Thunk's.
--
-- @since 0.1.0.0
newtype Registers ctx = Registers
  {unRegisters :: RecT (Thunk ctx) ctx}

-- | Construct a 'Registers' of unrelated primed variables.
--
-- @since 0.1.0.0
emptyRegisters :: Rec ctx -> Registers ctx
emptyRegisters = Registers . fieldMap (const Unchanged)

-- | Retrieves the value of the variable named @s@ in 'Registers' as a 'Thunk'.
--
-- @since 0.1.0.0
getRegister :: s # a .| ctx => Name s -> Registers ctx -> Thunk ctx a
getRegister n (Registers rs) = getRecT n rs

-- | Sets the value of the variable named @s@ to the result given by evaluating a 'Thunk'.
--
-- @since 0.1.0.0
setRegister :: s # a .| ctx => Name s -> a -> Registers ctx -> Registers ctx
setRegister n x (Registers rs) = Registers (setRecT n (Evaluated x) rs)

-- | Sets the value of the variable named @s@ in 'Registers' to an unevaluated expression.
--
-- @since 0.1.0.0
setThunk ::
  s # a .| ctx =>
  Name s ->
  Lang ctx '[Prime, Plain, NonDet, Error RuntimeException] a ->
  Registers ctx ->
  Registers ctx
setThunk n m (Registers rs) = Registers (setRecT n (Thunk m) rs)

-- | A 'Thunk' is the state of a primed variable in @ctx@.
--
-- * 'Thunk' is an unevaluated expression on the right-hand side of a closure.
-- * 'Evaluated' result of evaluating a thunk in current world.
-- * Any primed variable is implicitly 'Unchanged' if it has not been related in the current world.
--
-- @since 0.1.0.0
data Thunk ctx a
  = Thunk (Lang ctx '[Prime, Plain, NonDet, Error RuntimeException] a)
  | Evaluated a
  | Unchanged

-- | @since 0.1.0.0
instance Show a => Show (Thunk (s # a ': ctx) a) where
  show (Thunk _) = "<<thunk>>"
  show (Evaluated x) = show x
  show Unchanged = "<<unchanged>>"
