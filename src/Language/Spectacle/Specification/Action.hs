{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

-- |
--
-- @since 0.1.0.0
module Language.Spectacle.Specification.Action
  ( -- *
    Fairness (Unfair, WeakFair, StrongFair),
    ReflectFair,
    reflectFair,

    -- *
    type (\/) ((:\/:)),
    type (!>) (UnfairAction, WeakFairAction, StrongFairAction),

    -- *
    ActionInfo (ActionInfo),
    actionInfoFairness,

    -- *
    ActionSet (ActionSet),
    actionSetName,
    actionSetWorlds,

    -- *
    ActionSpine (ActionSpineNil, ActionSpineCon),
    spineToActionInfo,
    spineToActionSets,
    HasActions,
    takeActionSpine,
    type ActionCtxt,
  )
where

import Data.Hashable (Hashable)
import Data.Kind (Constraint, Type)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import Language.Spectacle.Exception.RuntimeException (RuntimeException)

import Data.Context (Context)
import Data.Name (Name)
import Data.Type.Rec (Rec)
import Data.World (World (World))
import Language.Spectacle.AST.Action (Action, runAction, runExceptionalAction)

-- ---------------------------------------------------------------------------------------------------------------------

infixr 5 \/, :\/:
infix 6 !>

-- | An enumeration of action fairness kinds.
--
-- @since 0.1.0.0
data Fairness
  = Unfair
  | WeakFair
  | StrongFair
  deriving (Eq, Ord, Enum, Show)

type ReflectFair :: Fairness -> Constraint
class ReflectFair fair where
  reflectFair :: Fairness

-- | @since 0.1.0.0
instance ReflectFair 'Unfair where
  reflectFair = Unfair

-- | @since 0.1.0.0
instance ReflectFair 'WeakFair where
  reflectFair = WeakFair

-- | @since 0.1.0.0
instance ReflectFair 'StrongFair where
  reflectFair = StrongFair

-- | Action disjunction.
--
-- @since 0.1.0.0
type (\/) :: forall k1 k2. (Context -> k1) -> (Context -> k2) -> Context -> Type
data (\/) :: forall k1 k2. (Context -> k1) -> (Context -> k2) -> Context -> Type where
  (:\/:) :: f ctxt -> g ctxt -> (f \/ g) ctxt

-- | Action declarations.
--
-- @since 0.1.0.0
data (!>) :: forall k1. k1 -> Fairness -> Context -> Type where
  UnfairAction ::
    Name s ->
    Action ctxt Bool ->
    (s !> 'Unfair) ctxt
  WeakFairAction ::
    Name s ->
    Action ctxt Bool ->
    (s !> 'WeakFair) ctxt
  StrongFairAction ::
    Name s ->
    Action ctxt Bool ->
    (s !> 'StrongFair) ctxt

actionDeclName :: (name !> fairness) ctxt -> String
actionDeclName = \case
  UnfairAction name _ -> show name
  WeakFairAction name _ -> show name
  StrongFairAction name _ -> show name

actionDeclFairness :: forall name fairness ctxt. ReflectFair fairness => (name !> fairness) ctxt -> Fairness
actionDeclFairness _ = reflectFair @fairness

fromActionDecl :: (name !> fairness) ctxt -> Action ctxt Bool
fromActionDecl = \case
  UnfairAction _ x -> x
  WeakFairAction _ x -> x
  StrongFairAction _ x -> x

-- ---------------------------------------------------------------------------------------------------------------------

newtype ActionInfo = ActionInfo
  {actionInfoFairness :: Fairness}
  deriving (Show)

type ActionSet :: Context -> Type
data ActionSet ctxt = ActionSet
  { actionSetName :: String
  , actionSetWorlds :: Set (World ctxt)
  }
  deriving (Eq, Ord)

-- | @since 0.1.0.0
deriving instance Show (Rec ctxt) => Show (ActionSet ctxt)

data ActionSpine :: Context -> [Type] -> Type where
  ActionSpineNil ::
    ActionSpine ctxt '[]
  ActionSpineCon ::
    ReflectFair fair =>
    (action !> fair) ctxt ->
    ActionSpine ctxt spine ->
    ActionSpine ctxt ((action !> fair) ctxt ': spine)

concatSpine :: ActionSpine ctxt actions -> ActionSpine ctxt actions' -> ActionSpine ctxt (actions ++ actions')
concatSpine ActionSpineNil sp' = sp'
concatSpine (ActionSpineCon x xs) sp' = ActionSpineCon x (concatSpine xs sp')

spineToActionInfo ::
  forall ctxt actions.
  ActionSpine ctxt actions ->
  Map String ActionInfo
spineToActionInfo = go Map.empty
  where
    go :: Map String ActionInfo -> ActionSpine ctxt actions' -> Map String ActionInfo
    go info = \case
      ActionSpineNil -> info
      ActionSpineCon actionDecl sp ->
        let declName = actionDeclName actionDecl
            declFair = actionDeclFairness actionDecl
         in go (Map.insert declName (ActionInfo declFair) info) sp

spineToActionSets ::
  forall ctxt actions.
  Hashable (Rec ctxt) =>
  World ctxt ->
  ActionSpine ctxt actions ->
  Either RuntimeException [ActionSet ctxt]
spineToActionSets (World _ state) = go
  where
    go :: ActionSpine ctxt actions' -> Either RuntimeException [ActionSet ctxt]
    go = \case
      ActionSpineNil -> Right []
      ActionSpineCon actionDecl sp -> do
        worlds <- runExceptionalAction state (fromActionDecl actionDecl)
        actionSets <- go sp
        return (ActionSet (actionDeclName actionDecl) worlds : actionSets)

type HasActions :: Context -> (Context -> Type) -> Constraint
class HasActions ctxt a where
  type ActionCtxt ctxt a :: [Type]

  takeActionSpine :: a ctxt -> ActionSpine ctxt (ActionCtxt ctxt a)

-- | @since 0.1.0.0
instance (HasActions ctxt a, HasActions ctxt b) => HasActions ctxt (a \/ b) where
  type ActionCtxt ctxt (a \/ b) = ActionCtxt ctxt a ++ ActionCtxt ctxt b

  takeActionSpine (xs :\/: ys) = concatSpine (takeActionSpine xs) (takeActionSpine ys)
  {-# INLINE CONLIKE takeActionSpine #-}

-- | @since 0.1.0.0
instance ReflectFair fair => HasActions ctxt (name !> fair) where
  type ActionCtxt ctxt (name !> fair) = '[(name !> fair) ctxt]

  takeActionSpine x = ActionSpineCon x ActionSpineNil
  {-# INLINE CONLIKE takeActionSpine #-}

type (++) :: [k] -> [k] -> [k]
type family xs ++ ys where
  '[] ++ ys = ys
  (x ': xs) ++ ys = x ': (xs ++ ys)
