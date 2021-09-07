{-# LANGUAGE TypeFamilies #-}

-- |
--
-- @since 0.1.0.0
module Data.Context
  ( -- * Contexts
    Context (CtxtNil, CtxtCon),

    -- ** Context Kinds
    CNil,
    type (:<),

    -- ** Context Families
    type CtxtCat,

    -- * Context Carriers
    Contextual,
    type Ctxt,
  )
where

import Data.Kind
import GHC.TypeLits

import Data.Ascript

-- ---------------------------------------------------------------------------------------------------------------------

data Context :: Type where
  CtxtNil :: Context
  CtxtCon :: Ascribe Symbol Type -> Context -> Context

type CNil :: Context
type CNil = 'CtxtNil

infixr 5 :<
type (:<) :: Ascribe Symbol Type -> Context -> Context
type x :< xs = 'CtxtCon x xs

type CtxtCat :: Context -> Context -> Context
type family CtxtCat xs ys where
  CtxtCat CNil ys = ys
  CtxtCat (x :< xs) ys = x :< CtxtCat xs ys

type Contextual :: Type -> Constraint
class Contextual a where
  type Ctxt a :: Context
