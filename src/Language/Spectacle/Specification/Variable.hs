{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

-- |
--
-- @since 0.1.0.0
module Language.Spectacle.Specification.Variable
  ( -- *
    Var ((:=)),
    type (:.) ((:.)),
    HasVariables,
    type VariableCtxt,
    takeInitialActions,
  )
where

import Data.Kind (Type)
import GHC.TypeLits (Symbol)

import Data.Context (CNil, Context, CtxtCat, type (:<))
import Data.Type.Rec (Name, RecT (RConT, RNilT), type (#))
import qualified Data.Type.Rec as Rec
import Language.Spectacle.Lang (Lang)
import Language.Spectacle.Syntax.NonDet (NonDet)

-- ---------------------------------------------------------------------------------------------------------------------

data Var :: Symbol -> Type -> Type where
  (:=) :: Name s -> Lang CNil '[NonDet] a -> Var s a

infixr 5 :.
data (:.) :: forall k1 k2. k1 -> k2 -> Type where
  (:.) :: a -> b -> a :. b

class HasVariables a where
  type VariableCtxt a :: Context

  takeInitialActions :: VariableCtxt a ~ ctxt => a -> RecT (Lang CNil '[NonDet]) ctxt

-- | @since 0.1.0.0
instance (HasVariables a, HasVariables b) => HasVariables (a :. b) where
  type VariableCtxt (a :. b) = CtxtCat (VariableCtxt a) (VariableCtxt b)

  takeInitialActions (acts1 :. acts2) = Rec.concat (takeInitialActions acts1) (takeInitialActions acts2)
  {-# INLINE takeInitialActions #-}

-- | @since 0.1.0.0
instance HasVariables (Var var ty) where
  type VariableCtxt (Var var ty) = (var # ty) :< CNil

  takeInitialActions (name := act) = RConT name act RNilT
  {-# INLINE takeInitialActions #-}
