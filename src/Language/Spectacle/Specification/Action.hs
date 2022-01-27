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

-- actionDeclName :: ActionType nm fair -> String
-- actionDeclName = \case
--   ActionUF name -> show name
--   ActionWF name -> show name
--   ActionSF name -> show name

-- fromActionDecl :: ActionType nm fair -> Action ctx Bool
-- fromActionDecl = \case
--   ActionUF _ action -> action
--   ActionWF _ action -> action
--   ActionSF _ action -> action

-- ---------------------------------------------------------------------------------------------------------------------

-- data Spine :: [Type] -> Type where
--   SpNil :: Spine ctx '[]
--   SpCon :: ActionType nm fair -> Spine ctx sp -> Spine (ActionType nm fair ': sp)

-- concatSpine :: Spine actions -> Spine ctx actions' -> Spine ctx (actions ++ actions')
-- concatSpine SpNil sp' = sp'
-- concatSpine (SpCon x xs) sp' = SpCon x (concatSpine xs sp')

-- spineToActionInfo :: Spine ctx acts -> Map String Fairness
-- spineToActionInfo = go Map.empty
--   where
--     go info = \case
--       SpNil -> info
--       SpCon (action :: ActionType nm fair ctx') sp ->
--         let declName = actionDeclName action
--             declFair = reifyFairness @fair
--          in go (Map.insert declName declFair info) sp

-- spineToActionSets ::
--   forall ctx actions.
--   Hashable (Rec ctx) =>
--   World ctx ->
--   Spine ctx actions ->
--   Either RuntimeException [ActionSet ctx]
-- spineToActionSets (World _ state) = go
--   where
--     go :: Spine ctx actions' -> Either RuntimeException [ActionSet ctx]
--     go = \case
--       SpNil -> Right []
--       SpCon actionDecl sp -> do
--         worlds <- runExceptionalAction state (fromActionDecl actionDecl)
--         actionSets <- go sp
--         return (ActionSet (actionDeclName actionDecl) worlds : actionSets)

-- type HasActs :: [Ascribe Symbol Type] -> ([Ascribe Symbol Type] -> Type) -> Constraint
-- class HasActs ctx t where
--   type ActionCtx ctx t :: [Type]

--   takeSpine :: t ctx -> Spine ctx (ActionCtx ctx t)

-- -- | @since 0.1.0.0
-- instance (HasActs ctx a, HasActs ctx b) => HasActs ctx (a \/ b) where
--   type ActionCtx ctx (a \/ b) = ActionCtx ctx a ++ ActionCtx ctx b

--   takeSpine (xs :\/: ys) = concatSpine (takeSpine xs) (takeSpine ys)
--   {-# INLINE CONLIKE takeSpine #-}

-- -- | @since 0.1.0.0
-- instance HasActs ctx (ActionType nm fair) where
--   type ActionCtx ctx (ActionType nm fair) = '[ActionType nm fair ctx]

--   takeSpine x = SpCon x SpNil
--   {-# INLINE CONLIKE takeSpine #-}
