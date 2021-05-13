module Language.Spectacle.AST.Action.Internal
  ( type Action,
    type ActionSyntax,
  )
where

import Data.Kind (Type)
import GHC.TypeLits (Symbol)

import Data.Type.Rec (Ascribe)
import Language.Spectacle.Exception.RuntimeException (RuntimeException)
import Language.Spectacle.Lang (EffectK, Lang)
import Language.Spectacle.Syntax.Closure.Internal (Closure)
import Language.Spectacle.Syntax.Error.Internal (Error)
import Language.Spectacle.Syntax.Logic.Internal (Logic)
import Language.Spectacle.Syntax.NonDet.Internal (NonDet)
import Language.Spectacle.Syntax.Plain.Internal (Plain)
import Language.Spectacle.Syntax.Quantifier.Internal (Quantifier)

-- -------------------------------------------------------------------------------------------------

type Action :: [Ascribe Symbol Type] -> Type -> Type
type Action ctx a = Lang ctx ActionSyntax a

type ActionSyntax :: [EffectK]
type ActionSyntax =
  '[ Closure
   , Quantifier
   , Logic
   , Plain
   , NonDet
   , Error RuntimeException
   ]
