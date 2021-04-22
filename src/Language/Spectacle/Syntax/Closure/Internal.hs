{-# LANGUAGE TypeFamilies #-}

module Language.Spectacle.Syntax.Closure.Internal
  ( Closure (Closure),
    Effect (Close),
  )
where

import Data.Void (Void)

import Data.Ascript (type (#))
import Data.Name (Name)
import Data.Type.Rec (type (.|))
import Language.Spectacle.Exception.RuntimeException (RuntimeException)
import Language.Spectacle.Lang (Effect, EffectK, Lang, ScopeK)
import Language.Spectacle.Syntax.Error.Internal (Error)
import Language.Spectacle.Syntax.NonDet.Internal (NonDet)
import Language.Spectacle.Syntax.Substitute.Internal (Plain, Prime)

-- -------------------------------------------------------------------------------------------------

newtype Closure :: EffectK where
  Closure :: Void -> Closure a

data instance Effect Closure :: ScopeK where
  Close ::
    (m ~ Lang ctx effs, s # a .| ctx) =>
    Name s ->
    Lang ctx '[Prime, Plain, NonDet, Error RuntimeException] a ->
    Effect Closure m ()
