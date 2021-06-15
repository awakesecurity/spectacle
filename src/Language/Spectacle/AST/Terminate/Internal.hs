module Language.Spectacle.AST.Terminate.Internal
  ( -- * Termination Condition Syntax
    type Terminate,
    type TerminateSyntax,
  )
where

import Data.Kind (Type)
import GHC.TypeLits (Symbol)

import Data.Type.Rec (Ascribe)
import Language.Spectacle.Lang (EffectK, Lang)
import Language.Spectacle.Syntax.Enabled.Internal (Enabled)
import Language.Spectacle.Syntax.Plain.Internal (Plain)

-- ---------------------------------------------------------------------------------------------------------------------

type Terminate :: [Ascribe Symbol Type] -> Type -> Type
type Terminate ctx a = Lang ctx TerminateSyntax a

type TerminateSyntax :: [EffectK]
type TerminateSyntax =
  '[ Plain
   , Enabled
   ]
