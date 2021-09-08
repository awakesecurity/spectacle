module Language.Spectacle.AST.Invariant.Internal
  ( type Invariant,
    type InvariantSyntax,
  )
where

import Data.Kind (Type)
import GHC.TypeLits (Symbol)

import Data.Type.Rec (Ascribe)
import Language.Spectacle.Exception.RuntimeException (RuntimeException)
import Language.Spectacle.Lang (EffectK, Lang)
import Language.Spectacle.Syntax.Enabled.Internal (Enabled)
import Language.Spectacle.Syntax.Error.Internal (Error)
import Language.Spectacle.Syntax.Logic.Internal (Logic)
import Language.Spectacle.Syntax.Modal.Internal (Modal)
import Language.Spectacle.Syntax.Plain.Internal (Plain)
import Language.Spectacle.Syntax.Prime.Internal (Prime)
import Data.Context

-- -------------------------------------------------------------------------------------------------

type Invariant :: Context -> Type -> Type
type Invariant ctxt a = Lang ctxt InvariantSyntax a

type InvariantSyntax :: [EffectK]
type InvariantSyntax =
  '[ Modal
   , Logic
   , Enabled
   , Prime
   , Plain
   , Error RuntimeException
   ]
