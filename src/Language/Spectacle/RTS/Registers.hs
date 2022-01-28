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
    StateFun,
    type StateFunSyntax,
    Thunk (Thunk, Evaluated, Unchanged),
  )
where

import Data.Functor.Identity (Identity (Identity))
import Data.Kind (Type)
import GHC.TypeLits (Symbol)

import Data.Ascript (type (#))
import Data.Type.Rec (Ascribe, Has, Name, Rec, RecF)
import qualified Data.Type.Rec as Rec
import Language.Spectacle.Exception.RuntimeException (RuntimeException)
import Language.Spectacle.Lang (EffectK, Lang)
import Language.Spectacle.Syntax.Error.Internal (Error)
import Language.Spectacle.Syntax.NonDet.Internal (NonDet)
import Language.Spectacle.Syntax.Plain.Internal (Plain)
import Language.Spectacle.Syntax.Prime.Internal (Prime)

-- ---------------------------------------------------------------------------------------------------------------------

type StateFun :: [Ascribe Symbol Type] -> Type -> Type
type StateFun ctx = Lang ctx StateFunSyntax

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
  {unRegisters :: RecF (Thunk ctxt) ctxt}

deriving instance Show (RecF (Thunk ctxt) ctxt) => Show (Registers ctxt)

-- | Construct a 'Registers' of unrelated primed variables.
--
-- @since 0.1.0.0
emptyRegisters :: Rec ctx -> Registers ctx
emptyRegisters = Registers . Rec.mapF \_ (Identity _) -> Unchanged

-- | Retrieves the value of the variable named @s@ in 'Registers' as a 'Thunk'.
--
-- @since 0.1.0.0
getRegister :: Has s a ctx => Name s -> Registers ctx -> Thunk ctx a
getRegister n (Registers rs) = Rec.getF n rs

-- | Sets the value of the variable named @s@ to the result given by evaluating a 'Thunk'.
--
-- @since 0.1.0.0
setRegister :: Has s a ctx => Name s -> a -> Registers ctx -> Registers ctx
setRegister n x (Registers rs) = Registers (Rec.setF n (Evaluated x) rs)

-- | Sets the value of the variable named @s@ in 'Registers' to an unevaluated expression.
--
-- @since 0.1.0.0
setThunk :: Has s a ctx => Name s -> StateFun ctx a -> Registers ctx -> Registers ctx
setThunk n m (Registers rs) = Registers (Rec.setF n (Thunk m) rs)

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
instance Show a => Show (Thunk (s # a ': ctx) a) where
  show (Thunk _) = "<<thunk>>"
  show (Evaluated x) = show x
  show Unchanged = "<<unchanged>>"
