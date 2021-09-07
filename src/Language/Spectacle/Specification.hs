{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Language.Spectacle.Specification
  ( -- *
    Spec (Spec),
    Specification,
    specInitialWorlds,



    -- Spec (Spec),
    -- specInit,
    -- specNext,

    -- -- *
    -- ActionCtxt (ActionCtxtNil, ActionCtxtCon),
    -- AnAction (ActionHere, ActionThere),
    -- (\/),
    -- endOfActions,
    -- acts,
    -- ActionInfo(ActionInfo),
    -- actionInfoName,
    -- actionInfoFair,

    -- -- *
    -- PropInfo(PropInfo),
    -- propInfoIsAlways,
    -- propInfoIsEventually,
    -- propInfoLeadsTo,
    -- propInfoIsInfinitelyOften,
    -- propInfoIsStaysAs,
    -- Always,
    -- Eventually,
    -- type (/\),
    -- type (==>),
    -- IsFormula,
  )
where

import Data.Kind
import Data.Function
import Data.Functor
import Data.Hashable
import Data.Proxy
import GHC.TypeLits
import Data.Map.Strict (Map)
import Data.Set (Set)
import qualified Data.Set as Set
import qualified Data.Map.Strict as Map

import Data.Ascript
import Data.Type.Rec
import Data.World
import Language.Spectacle.AST
import Language.Spectacle.AST.Action
import Language.Spectacle.Lang
import Language.Spectacle.Syntax.NonDet
import Language.Spectacle.Specification.Action
import Language.Spectacle.Specification.Prop
import Language.Spectacle.Specification.Variable
import Data.Context

-- ---------------------------------------------------------------------------------------------------------------------

type Spec :: Type -> (Context -> Type) -> (Context -> Type -> Type) -> Type
data Spec ctxt spec prop where
  Spec ::
    ctxt ->
    spec (VariableCtxt ctxt) ->
    Spec ctxt spec prop

type Specification :: Type -> (Context -> Type) -> (Context -> Type -> Type) -> Constraint
type Specification ctxt spec prop =
  ( HasVariables ctxt
  , HasActions (VariableCtxt ctxt) spec
  , HasProp prop
  )

-- | @since 0.1.0.0
instance HasVariables ctxt => HasVariables (Spec ctxt actions prop) where
  type VariableCtxt (Spec ctxt actions prop) = VariableCtxt ctxt

  takeInitialActions (Spec ctxt _) = takeInitialActions ctxt
  {-# INLINE takeInitialActions #-}

specInitialWorlds ::
  forall vars ctxt spec prop.
  (HasVariables vars, VariableCtxt vars ~ ctxt, Hashable (Rec ctxt)) =>
  Spec vars spec prop ->
  Set (World ctxt)
specInitialWorlds (Spec ctxt _) =
  takeInitialActions ctxt
    & fieldMap (runLang . runNonDetA @[])
    & seqRec
    & foldMap (Set.singleton . makeWorld)
  where
    seqRec :: RecT [] ctxt' -> [Rec ctxt']
    seqRec = \case
      RNilT -> [RNil]
      RConT name states xs -> do
        var <- states
        vars <- seqRec xs
        return (RCon name var vars)


-- type Spec :: Context -> [Act Symbol Fairness] -> k -> Type
-- data Spec sig acts prop where
--   Spec ::
--     { specInit :: Initial sig ()
--     , specNext :: ActionCtxt sig acts
--     } ->
--     Spec vars acts prop

-- data ActionInfo = ActionInfo
--   { actionInfoName :: String
--   , actionInfoFair :: Fairness
--   }

-- specActionInfo :: Spec sig acts prop -> [ActionInfo]
-- specActionInfo (Spec _ acts) = actionCtxtNames acts

-- type ActionCtxt :: Context -> [Act Symbol Fairness] -> Type
-- data ActionCtxt sig acts where
--   ActionCtxtNil :: ActionCtxt sig acts
--   ActionCtxtCon :: AnAction sig acts -> ActionCtxt sig acts -> ActionCtxt sig acts

-- actionCtxtNames :: ActionCtxt ctxt acts -> [ActionInfo]
-- actionCtxtNames ActionCtxtNil = []
-- actionCtxtNames (ActionCtxtCon act acts) = takeInfoAnAction act : actionCtxtNames acts

-- type AnAction :: Context -> [Act Symbol Fairness] -> Type
-- data AnAction ctxt acts where
--   ActionHere ::
--     (ReflectFairness fair, KnownSymbol name) =>
--     Proxy fair ->
--     Action (name !> fair) ctxt Bool ->
--     AnAction ctxt (name !> fair ': acts)
--   ActionThere ::
--     AnAction ctxt acts ->
--     AnAction ctxt (act ': acts)

-- takeInfoAnAction :: AnAction acts sig -> ActionInfo
-- takeInfoAnAction = \case
--   ActionHere fair action -> ActionInfo (reflectActionName action) (reflectFairness fair)
--   ActionThere actions -> takeInfoAnAction actions

-- type ReflectFairness :: Fairness -> Constraint
-- class ReflectFairness fair where
--   reflectFairness :: Proxy fair -> Fairness

-- -- | @since 0.1.0.0
-- instance ReflectFairness 'Unfair where
--   reflectFairness _ = Unfair
--   {-# INLINE CONLIKE reflectFairness #-}

-- -- | @since 0.1.0.0
-- instance ReflectFairness 'WeakFair where
--   reflectFairness _ = WeakFair
--   {-# INLINE CONLIKE reflectFairness #-}

-- -- | @since 0.1.0.0
-- instance ReflectFairness 'StrongFair where
--   reflectFairness _ = StrongFair
--   {-# INLINE CONLIKE reflectFairness #-}

-- infixr 5 \/

-- (\/) :: HasAction act acts => Action act sig Bool -> ActionCtxt sig acts -> ActionCtxt sig acts
-- action \/ actions = ActionCtxtCon (acts action) actions

-- endOfActions :: ActionCtxt sig acts
-- endOfActions = ActionCtxtNil

-- type HasAction :: Act Symbol Fairness -> [Act Symbol Fairness] -> Constraint
-- class HasAction act acts where
--   acts :: Action act ctxt Bool -> AnAction ctxt acts

-- -- | @since 0.1.0.0
-- instance {-# OVERLAPS #-} (ReflectFairness fair, KnownSymbol name) => HasAction (name !> fair) (name !> fair ': acts) where
--   acts action = ActionHere Proxy action
--   {-# INLINE CONLIKE acts #-}

-- -- | @since 0.1.0.0
-- instance HasAction act acts => HasAction act (act' ': acts) where
--   acts = ActionThere . acts
--   {-# INLINE CONLIKE acts #-}

-- class ReflectActions acts where
--   reflectActionNames :: [String]

-- instance (KnownSymbol name, ReflectActions acts) => ReflectActions (name !> fair ': acts) where
--   reflectActionNames = symbolVal (Proxy @name) : reflectActionNames @acts

-- instance ReflectActions '[] where
--   reflectActionNames = []

-- type Vars :: [Ascribe Symbol Type] -> Type
-- data Vars sig

-- data Globally acts = Globally acts

-- data Future acts = Future acts

-- data And prop1 prop2 = And prop1 prop2

-- data Implies prop1 prop2 = Implies prop1 prop2

-- type Always acts = 'Globally acts

-- type Eventually acts = 'Future acts

-- type InfinitelyOften acts = 'Globally ('Future acts)

-- type StaysAs acts = 'Future ('Globally acts)

-- type prop1 /\ prop2 = 'And prop1 prop2

-- type (==>) :: forall k1 k2. k1 ->  k2 -> Implies k1 k2
-- type p1 ==> p2 = 'Implies p1 p2

-- data PropInfo = PropInfo
--   { propInfoIsAlways :: Bool
--   , propInfoIsEventually :: Bool
--   , propInfoLeadsTo :: Set String
--   , propInfoIsInfinitelyOften :: Bool
--   , propInfoIsStaysAs :: Bool
--   }

-- propInfoAlways :: PropInfo
-- propInfoAlways = PropInfo True False Set.empty False False

-- propInfoEventually :: PropInfo
-- propInfoEventually = PropInfo False True Set.empty False False

-- propInfoLeads :: Set String -> PropInfo
-- propInfoLeads leadsToSet = PropInfo False False leadsToSet False False

-- propInfoInfinitelyOften :: PropInfo
-- propInfoInfinitelyOften = PropInfo False False Set.empty True False

-- propInfoStaysAs :: PropInfo
-- propInfoStaysAs = PropInfo False False Set.empty False True

-- instance Semigroup PropInfo where
--   PropInfo g1 f1 l1 io1 sa1 <> PropInfo g2 f2 l2 io2 sa2 =
--     PropInfo (g1 || g2) (f1 || f2) (l1 <> l2) (io1 || io2) (sa1 || sa2)

-- instance Monoid PropInfo where
--   mempty = PropInfo False False Set.empty False False

-- class IsFormula a where
--   formulaInfo :: Map String PropInfo

-- instance ReflectActions acts => IsFormula (Always acts) where
--   formulaInfo = foldr f Map.empty (reflectActionNames @acts)
--     where
--       f action = Map.unionWith (<>) (Map.singleton action propInfoAlways)

-- instance ReflectActions acts => IsFormula (Eventually acts) where
--   formulaInfo = foldr f Map.empty (reflectActionNames @acts)
--     where
--       f action = Map.unionWith (<>) (Map.singleton action propInfoEventually)

-- instance (KnownSymbol act, ReflectActions acts) => IsFormula (Always (act ==> Eventually acts)) where
--   formulaInfo = Map.singleton (symbolVal (Proxy @act)) (propInfoLeads (Set.fromList (reflectActionNames @acts)))

-- instance ReflectActions acts => IsFormula (InfinitelyOften acts) where
--   formulaInfo = foldr f Map.empty (reflectActionNames @acts)
--     where
--       f action = Map.unionWith (<>) (Map.singleton action propInfoInfinitelyOften)

-- instance ReflectActions acts => IsFormula (StaysAs acts) where
--   formulaInfo = foldr f Map.empty (reflectActionNames @acts)
--     where
--       f action = Map.unionWith (<>) (Map.singleton action propInfoStaysAs)

-- instance (IsFormula prop1, IsFormula prop2) => IsFormula (prop1 /\ prop2) where
--   formulaInfo = Map.unionWith (<>) (formulaInfo @prop1) (formulaInfo @prop2)
