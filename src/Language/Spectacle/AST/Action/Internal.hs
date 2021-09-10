{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Language.Spectacle.AST.Action.Internal
  ( -- *
    Action (Action, getAction),
    type ActionSyntax,
  )
where

import Control.Applicative
import Data.Kind (Type)

import Data.Context
import Data.Type.Rec (type (#), type (.|))
import Language.Spectacle.Exception.RuntimeException (RuntimeException)
import Language.Spectacle.Lang (EffectK, Lang, scope, Member)
import Language.Spectacle.Syntax.Closure.Internal
import Language.Spectacle.Syntax.Error.Internal (Error)
import Language.Spectacle.Syntax.Logic.Internal (Logic)
import Language.Spectacle.Syntax.NonDet.Internal (NonDet)
import Language.Spectacle.Syntax.Plain.Internal
import Language.Spectacle.Syntax.Quantifier.Internal (Quantifier)

-- ---------------------------------------------------------------------------------------------------------------------

newtype Action :: Context -> Type -> Type where
  Action :: {getAction :: Lang ctxt ActionSyntax a} -> Action ctxt a
  deriving (Functor, Applicative, Monad, Alternative)

instance Contextual (Action ctxt a) where
  type Ctxt (Action ctxt a) = ctxt

-- | @since 0.1.0.0
instance (s # a .| ctxt) => PlainIntro (Action ctxt) s a where
  plainIntro name = Action (scope (PlainVar name))
  {-# INLINE plainIntro #-}

-- | @since 0.1.0.0
instance (s # a .| ctxt) => ClosureIntro (Action ctxt) s a where
  closureIntro name expr = Action (scope (Close name expr))

type ActionSyntax :: [EffectK]
type ActionSyntax =
  -- NOTE: 'Closure' must be handled before 'Quantifier'. If 'Quantifier' discharged before 'Closure', erroneous values
  -- are produced from any 'Closure' nested within a forall/exists.
  '[ Logic
   , Closure
   , Quantifier
   , Plain
   , NonDet
   , Error RuntimeException
   ]
