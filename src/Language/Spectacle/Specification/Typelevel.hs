{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

-- |
--
-- @since 0.1.0.0
module Language.Spectacle.Specification.Typelevel
  ( -- *
    Spec (Spec),
    Theorem (Theorem),
    Var((:=)),
    (:.)((:.)),
    type (\/)((:\/:)),
    type (/\)((:/\:)),
    (:>)(UnfairAction, WeakFairAction),
    HasVariables,
    type VariableCtxt,
    HasActions,
    type ActionCtxt,
  )
where

import Data.Kind
import GHC.TypeLits

import Data.Type.Rec
import Data.Context
import Language.Spectacle.Lang
import Language.Spectacle.Syntax.NonDet
import Language.Spectacle.AST.Action hiding (Act)
import Language.Spectacle.AST.Initial
import qualified Language.Spectacle.AST.Action as A

-- ---------------------------------------------------------------------------------------------------------------------


data Theorem :: Type -> (Context -> [Act Symbol Fairness] -> Type) -> Type where
  Theorem ::
    spec ->
    prop (VariableCtxt spec) (ActionCtxt spec) ->
    Theorem spec prop


type (++) :: [k] -> [k] -> [k]
type family xs ++ ys where
  '[] ++ ys = ys
  (x ': xs) ++ ys = x ': (xs ++ ys)
