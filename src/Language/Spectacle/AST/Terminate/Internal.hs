module Language.Spectacle.AST.Terminate.Internal
  ( -- * Termination Condition Syntax
    type Terminate,
    type TerminateSyntax,
  )
where

import Data.Kind (Type)

import Data.Context (Context)
import Language.Spectacle.Lang (EffectK, Lang)
import Language.Spectacle.Syntax.Enabled.Internal (Enabled)
import Language.Spectacle.Syntax.Plain.Internal (Plain)

-- ---------------------------------------------------------------------------------------------------------------------

type Terminate :: Context -> Type -> Type
type Terminate ctx a = Lang ctx TerminateSyntax a

type TerminateSyntax :: [EffectK]
type TerminateSyntax =
  '[ Plain
   , Enabled
   ]
