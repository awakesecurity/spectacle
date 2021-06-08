module Language.Spectacle.AST.Invariant.Internal
  ( type Invariant,
    type InvariantSyntax,
  )
where

import Data.Kind (Type)
import GHC.TypeLits (Symbol)

import Data.Type.Rec
import Language.Spectacle.Exception.RuntimeException
import Language.Spectacle.Lang
import Language.Spectacle.Syntax.Error.Internal
import Language.Spectacle.Syntax.Logic.Internal
import Language.Spectacle.Syntax.Modal.Graded
import Language.Spectacle.Syntax.Modal.Internal
import Language.Spectacle.Syntax.Modal.Preterm
import Language.Spectacle.Syntax.Plain.Internal
import Language.Spectacle.Syntax.Prime.Internal
import Language.Spectacle.Syntax.Fresh.Internal

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
