{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Language.Spectacle.AST.Action.Internal
  ( -- *
    Action (Action, getAction),
    type ActionSyntax,
  )
where

import Control.Applicative (Alternative)
import Data.Kind (Type)

import Data.Context (Context, Contextual(Ctxt))
import Data.Type.Rec (Has)
import Language.Spectacle.Exception.RuntimeException (RuntimeException)
import Language.Spectacle.Lang (EffectK, Lang, scope)
import Language.Spectacle.Syntax.Closure.Internal
  ( Closure,
    ClosureIntro (closureIntro),
    Effect (Close),
  )
import Language.Spectacle.Syntax.Error.Internal (Error)
import Language.Spectacle.Syntax.Logic.Internal (Logic)
import Language.Spectacle.Syntax.NonDet.Internal (NonDet)
import Language.Spectacle.Syntax.Plain.Internal
  ( Effect (PlainVar),
    Plain,
    PlainIntro (plainIntro),
  )
import Language.Spectacle.Syntax.Quantifier.Internal
  ( Effect (Exists, Forall),
    Quantifier,
    QuantifierIntro,
    existsIntro,
    forallIntro,
  )

-- ---------------------------------------------------------------------------------------------------------------------

newtype Action :: Context -> Type -> Type where
  Action :: {getAction :: Lang ctxt ActionSyntax a} -> Action ctxt a
  deriving (Functor, Applicative, Monad, Alternative)

instance Contextual (Action ctxt a) where
  type Ctxt (Action ctxt a) = ctxt

-- | @since 0.1.0.0
instance (Has s a ctxt) => PlainIntro (Action ctxt) s a where
  plainIntro name = Action (scope (PlainVar name))
  {-# INLINE plainIntro #-}

-- | @since 0.1.0.0
instance (Has s a ctxt) => ClosureIntro (Action ctxt) s a where
  closureIntro name expr = Action (scope (Close name expr))

-- | @since 0.1.0.0
instance QuantifierIntro (Action ctxt) where
  forallIntro xs p = Action (scope (Forall xs (getAction . p)))

  existsIntro xs p = Action (scope (Exists xs (getAction . p)))

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
