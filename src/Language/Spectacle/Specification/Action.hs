{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

-- |
-- Module      :  Language.Spectacle.Specification.Action
-- Copyright   :  (c) Arista Networks, 2022-2023
-- License     :  Apache License 2.0, see LICENSE
--
-- Stability   :  stable
-- Portability :  non-portable (GHC extensions)
--
-- TODO: docs
--
-- @since 1.0.0
module Language.Spectacle.Specification.Action
  ( -- * ActionType
    ActionType (ActionUF, ActionWF, ActionSF),

    -- ** Projection
    toAction,
    toFairness,
  )
where

import Data.Kind (Type)
import GHC.TypeLits (Symbol)

import Data.Type.Rec (Ascribe)
import Language.Spectacle.AST.Action (Action)
import Language.Spectacle.Fairness (Fairness (StrongFair, Unfair, WeakFair), reifyFairness)

-- ---------------------------------------------------------------------------------------------------------------------

-- | Action declarations.
--
-- @since 1.0.0
data ActionType :: [Ascribe Symbol Type] -> Fairness -> Type where
  ActionUF :: Action ctx Bool -> ActionType ctx 'Unfair
  ActionWF :: Action ctx Bool -> ActionType ctx 'WeakFair
  ActionSF :: Action ctx Bool -> ActionType ctx 'StrongFair

toAction :: ActionType ctx fair -> Action ctx Bool
toAction (ActionUF act) = act
toAction (ActionWF act) = act
toAction (ActionSF act) = act

toFairness :: forall ctx fair. ActionType ctx fair -> Fairness
toFairness ActionUF {} = reifyFairness @fair
toFairness ActionWF {} = reifyFairness @fair
toFairness ActionSF {} = reifyFairness @fair
