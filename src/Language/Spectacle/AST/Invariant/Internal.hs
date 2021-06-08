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
import Language.Spectacle.Syntax.Error.Internal (Error)
import Language.Spectacle.Syntax.Fresh.Internal (Fresh)
import Language.Spectacle.Syntax.Logic.Internal (Logic)
import Language.Spectacle.Syntax.Modal.Internal (Modal)
import Language.Spectacle.Syntax.Plain.Internal (Plain)

-- -------------------------------------------------------------------------------------------------

type Invariant :: [Ascribe Symbol Type] -> Type -> Type
type Invariant ctx a = Lang ctx (InvariantSyntax ctx) a

type InvariantSyntax :: [Ascribe Symbol Type] -> [EffectK]
type InvariantSyntax ctx =
  '[ Modal
   , Logic
   , Plain
   , Fresh
   , Error RuntimeException
   ]
