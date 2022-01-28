{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

-- |
--
-- @since 0.1.0.0
module Language.Spectacle.Specification.Action
  ( -- * ActionType
    ActionType (ActionUF, ActionWF, ActionSF),

    -- ** Projection
    toAction,
    toFairness,
  )
where

import Data.Hashable (Hashable)
import Data.Kind (Constraint, Type)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import GHC.TypeLits (Symbol)
import Language.Spectacle.Exception.RuntimeException (RuntimeException)
import Type.Reflection (Typeable)

import Data.Name (Name)
import Data.Type.List (type (++))
import Data.Type.Rec (Ascribe, Rec)
import Data.World (World (World))
import Language.Spectacle.AST.Action (Action, runExceptionalAction)
import Language.Spectacle.Fairness

-- ---------------------------------------------------------------------------------------------------------------------

-- | Action declarations.
--
-- @since 0.1.0.0
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
