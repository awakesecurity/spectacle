module Language.Spectacle.RTS.Env
  ( -- * Environment effect
    Environment,
    environment,
    locally,
    runEnvironment,

    -- * Environments
    Env (..),
    setEnvNeutral,
    setEnvValue,
    emptyEnv,

    -- * Registers
    Registers (Registers, unRegisters),
    setRegisterValue,
    setRegisterNeutral,
    getRegisters,
    emptyRegisters,

    -- * Values
    Value (Value, Neutral),
  )
where

import Data.Kind (Type)
import GHC.TypeLits (Symbol)

import Language.Spectacle.Effect.NonDet (NonDet)
import Language.Spectacle.Lang
  ( Lang,
    Syntax,
    interpose,
    interpret,
    send,
    type (|>),
  )
import Language.Spectacle.RTS.Callstack (Callstack)
import Language.Spectacle.RTS.Exception (RuntimeError)
import Language.Spectacle.Syntax.Quantifier (Quantifier)
import Language.Spectacle.Syntax.Subst (Subst)
import Language.Spectacle.Type.Rec
  ( Ascribe,
    FieldT (FieldT),
    Name,
    Rec,
    RecT,
    overFields,
    pattern Field,
    type (#),
    type (<:) (..),
  )

-- -----------------------------------------------------------------------------

data Environment :: Syntax where
  Environment :: m ~ Lang sig cxt => Environment m (Env cxt)

environment :: Environment |> sig => Lang sig cxt (Env cxt)
environment = send Environment
{-# INLINE environment #-}

locally :: Environment |> sig => Env cxt -> Lang sig cxt a -> Lang sig cxt a
locally env = interpose \Environment -> pure env
{-# INLINE locally #-}

runEnvironment :: Env cxt -> Lang (Environment ': sig) cxt a -> Lang sig cxt a
runEnvironment env = interpret \Environment -> pure env
{-# INLINE runEnvironment #-}

-- -----------------------------------------------------------------------------

data Env cxt = Env
  { bindings :: Registers cxt
  , callstack :: Callstack
  }

emptyEnv :: Rec cxt -> Env cxt
emptyEnv record = Env (emptyRegisters record) mempty
{-# INLINE emptyEnv #-}

setEnvValue :: nm # a <: cxt => Name nm -> a -> Env cxt -> Env cxt
setEnvValue name x (Env binds stack) = Env (setRegisterValue name x binds) stack
{-# INLINE setEnvValue #-}

setEnvNeutral ::
  nm # a <: cxt =>
  Name nm ->
  Lang ExprPrototype cxt a ->
  Env cxt ->
  Env cxt
setEnvNeutral name expr (Env binds stack) =
  Env (setRegisterNeutral name expr binds) stack
{-# INLINE setEnvNeutral #-}

-- -----------------------------------------------------------------------------

type ExprPrototype =
  '[ Quantifier
   , Subst
   , Environment
   , NonDet
   , RuntimeError
   ]

type Registers :: [Ascribe Symbol Type] -> Type
newtype Registers cxt = Registers
  {unRegisters :: RecT (Value cxt) cxt}

emptyRegisters :: Rec cxt -> Registers cxt
emptyRegisters record =
  Registers $
    (\(Field name x) -> FieldT name (Value name x)) `overFields` record
{-# INLINE emptyRegisters #-}

getRegisters :: nm # a <: cxt => Name nm -> Registers cxt -> Value cxt a
getRegisters name (Registers record) = getRecT record name
{-# INLINE getRegisters #-}

setRegisterNeutral ::
  nm # a <: cxt =>
  Name nm ->
  Lang ExprPrototype cxt a ->
  Registers cxt ->
  Registers cxt
setRegisterNeutral name fn (Registers r) = Registers (putRecT r name (Neutral name fn))
{-# INLINE setRegisterNeutral #-}

setRegisterValue ::
  nm # a <: cxt =>
  Name nm ->
  a ->
  Registers cxt ->
  Registers cxt
setRegisterValue name x (Registers r) = Registers (putRecT r name (Value name x))
{-# INLINE setRegisterValue #-}

type Value :: [Ascribe Symbol Type] -> Type -> Type
data Value cxt a where
  Value ::
    {-# UNPACK #-} !(Name nm) ->
    a ->
    Value cxt a
  Neutral ::
    {-# UNPACK #-} !(Name nm) ->
    Lang ExprPrototype cxt a ->
    Value cxt a

instance Show a => Show (Value cxt a) where
  show (Value name x) = show name ++ ":=" ++ show x
  show (Neutral name _) = "<" ++ show name ++ " neutral>"
