{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

-- |
--
-- @since 0.1.0.0
module Language.Spectacle.Specification.Variable
  ( -- * Syntax
    Var ((:=)),
    type (:.) ((:.)),

    -- * Variable Constraints
    HasVars,
    HasVariables,
    type VarCtxt,
    runInitActions,
    runInitStates,
  )
where

import Control.Applicative (Applicative (liftA2))
import Data.Hashable (Hashable)
import Data.Kind (Constraint, Type)
import GHC.TypeLits (Symbol)

import Data.Context (CNil, Context, CtxtCat, type (:<))
import Data.Type.Rec (HasDict, Name, Rec, RecF (ConF, NilF), pattern Con, pattern Nil, type (#))
import qualified Data.Type.Rec as Rec
import Data.World (World, makeWorld)
import Language.Spectacle.Lang (Lang, runLang)
import Language.Spectacle.Syntax.NonDet (NonDet, runNonDetA)

infixr 5 :.

-- ---------------------------------------------------------------------------------------------------------------------

data Var :: Symbol -> Type -> Type where
  (:=) :: Name s -> Lang CNil '[NonDet] a -> Var s a

data (:.) :: Type -> Type -> Type where
  (:.) :: a -> b -> a :. b

type HasVars :: Type -> Context -> Constraint
type HasVars vars ctxt = (HasVariables vars, VarCtxt vars ~ ctxt)

class HasVariables a where
  type VarCtxt a :: Context

  runInitActions :: VarCtxt a ~ ctxt => a -> RecF (Lang CNil '[NonDet]) ctxt

-- | @since 0.1.0.0
instance (HasVariables a, HasVariables b) => HasVariables (a :. b) where
  type VarCtxt (a :. b) = CtxtCat (VarCtxt a) (VarCtxt b)

  runInitActions (xs :. ys) = Rec.concatF (runInitActions xs) (runInitActions ys)
  {-# INLINE runInitActions #-}

-- | @since 0.1.0.0
instance HasVariables (Var var ty) where
  type VarCtxt (Var var ty) = (var # ty) :< CNil

  runInitActions (name := act) = ConF name act NilF
  {-# INLINE runInitActions #-}

runInitStates :: forall vars ctx. (HasVars vars ctx, HasDict Hashable ctx) => vars -> [World ctx]
runInitStates vs = map makeWorld (go acts)
  where
    go :: RecF [] ctx' -> [Rec ctx']
    go NilF = [Nil]
    go (ConF nm xs rs) = liftA2 (Con nm) xs (go rs)

    runner :: Name s -> Lang ctx' '[NonDet] a -> [a]
    runner _ = runLang . runNonDetA @[]

    acts :: RecF [] ctx
    acts = Rec.mapF runner (runInitActions vs)
