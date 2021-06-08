module Language.Spectacle.AST.Initial.Internal
  ( -- * Initial State Syntax
    type Initial,
    type InitialSyntax,
    InitialValues (InitialValues),
    InitialValue (InitialValue),

    -- * Internal State
    emptyInitialValues,
    initializeValue,
  )
where

import Data.Kind (Type)
import GHC.TypeLits (Symbol)

import Data.Type.Rec
  ( Ascribe,
    HasSel (setRecT),
    Name,
    RecT,
    ReflectRow (repeatRow),
    type (#),
    type (.|),
  )
import Language.Spectacle.Exception.RuntimeException (RuntimeException)
import Language.Spectacle.Lang (EffectK, Lang)
import Language.Spectacle.Syntax.Closure.Internal (Closure, ClosureKind (InitialClosure))
import Language.Spectacle.Syntax.Error.Internal (Error)
import Language.Spectacle.Syntax.NonDet.Internal (NonDet)

-- -------------------------------------------------------------------------------------------------

type Initial :: [Ascribe Symbol Type] -> Type -> Type
type Initial ctx a = Lang ctx InitialSyntax a

type InitialSyntax :: [EffectK]
type InitialSyntax = '[Closure 'InitialClosure, NonDet, Error RuntimeException]

-- -------------------------------------------------------------------------------------------------

newtype InitialValues :: [Ascribe Symbol Type] -> Type where
  InitialValues :: RecT Maybe ctx -> InitialValues ctx

newtype InitialValue a = InitialValue (Maybe a)

-- | Constructs an empty 'InitialValues' filled with 'Nothing'.
--
-- @since 0.1.0.0
emptyInitialValues :: ReflectRow ctx => InitialValues ctx
emptyInitialValues = InitialValues (repeatRow Nothing)
{-# INLINE emptyInitialValues #-}

-- | Initializes the variable @'Name' s@ to the given value.
--
-- @since 0.1.0.0
initializeValue :: s # a .| ctx => Name s -> a -> InitialValues ctx -> InitialValues ctx
initializeValue name x (InitialValues r) = InitialValues (setRecT name (Just x) r)
{-# INLINE initializeValue #-}
