{-# LANGUAGE AllowAmbiguousTypes #-}

-- |
--
-- @since 0.1.0.0
module Language.Spectacle.Specification.Prop
  ( -- * Temporal Formula
    TemporalType (PropG, PropF, PropGF, PropFG),

    -- ** Projection
    toFormula,
    toModality,

    -- * Temporal Operators
    Modality (Always, Infinitely, Eventually, Stays),
  )
where

import Data.Kind (Type)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Proxy (Proxy (Proxy))
import Data.Set (Set)
import qualified Data.Set as Set
import GHC.TypeLits (KnownSymbol, Symbol, symbolVal)

import Data.Ascript
import Language.Spectacle.AST.Temporal

-- ---------------------------------------------------------------------------------------------------------------------

data TemporalType :: [Ascribe Symbol Type] -> Modality -> Type where
  PropG :: Temporal ctx Bool -> TemporalType ctx 'Always
  PropF :: Temporal ctx Bool -> TemporalType ctx 'Eventually
  PropGF :: Temporal ctx Bool -> TemporalType ctx 'Infinitely
  PropFG :: Temporal ctx Bool -> TemporalType ctx 'Stays

toFormula :: TemporalType ctx op -> Temporal ctx Bool
toFormula = \case
  PropG form -> form
  PropF form -> form
  PropGF form -> form
  PropFG form -> form

toModality :: TemporalType ctx op -> Modality
toModality = \case
  PropG {} -> Always
  PropF {} -> Eventually
  PropGF {} -> Infinitely
  PropFG {} -> Stays

data Modality
  = Always
  | Eventually
  | Infinitely
  | Stays

-- infixr 5 /\

-- type (/\) ::
--   ([Ascribe Symbol Type] -> Type -> Type) ->
--   ([Ascribe Symbol Type] -> Type -> Type) ->
--   [Ascribe Symbol Type] ->
--   Type ->
--   Type
-- data (/\) prop1 prop2 ctxt actions

-- type Always :: forall k. k -> [Ascribe Symbol Type] -> Type -> Type
-- data Always prop ctxt actions

-- type Eventually :: forall k. k -> [Ascribe Symbol Type] -> Type -> Type
-- data Eventually prop ctxt actions

-- type (~~>) :: forall k. k -> k -> [Ascribe Symbol Type] -> Type -> Type
-- data (p ~~> q) ctxt actions

-- -- ---------------------------------------------------------------------------------------------------------------------

-- class HasProp a where
--   collectPropInfo :: Map String PropInfo

-- -- | @since 0.1.0.0
-- instance KnownSymbol action => HasProp (Always action) where
--   collectPropInfo = Map.singleton (symbolVal (Proxy @action)) makePropInfoAlways
--   {-# INLINE CONLIKE collectPropInfo #-}

-- -- | @since 0.1.0.0
-- instance KnownSymbol action => HasProp (Always '[action]) where
--   collectPropInfo = Map.singleton (symbolVal (Proxy @action)) makePropInfoAlways
--   {-# INLINE CONLIKE collectPropInfo #-}

-- -- | @since 0.1.0.0
-- instance (KnownSymbol action, HasProp (Always (action' ': actions))) => HasProp (Always (action ': action' ': actions)) where
--   collectPropInfo = Map.unionWith (<>) (collectPropInfo @(Always action)) (collectPropInfo @(Always (action' ': actions)))
--   {-# INLINE CONLIKE collectPropInfo #-}

-- -- | @since 0.1.0.0
-- instance KnownSymbol action => HasProp (Eventually action) where
--   collectPropInfo = Map.singleton (symbolVal (Proxy @action)) makePropInfoEventually
--   {-# INLINE CONLIKE collectPropInfo #-}

-- -- | @since 0.1.0.0
-- instance (KnownSymbol action, HasProp (Eventually actions)) => HasProp (Eventually (action ': actions)) where
--   collectPropInfo = Map.unionWith (<>) (collectPropInfo @(Eventually action)) (collectPropInfo @(Eventually actions))
--   {-# INLINE CONLIKE collectPropInfo #-}

-- -- | @since 0.1.0.0
-- instance (KnownSymbol p, KnownSymbol q) => HasProp (p ~~> q) where
--   collectPropInfo = Map.singleton (symbolVal (Proxy @p)) (makePropInfoLeadsTo (Set.singleton (symbolVal (Proxy @q))))
--   {-# INLINE CONLIKE collectPropInfo #-}

-- -- | @since 0.1.0.0
-- instance (KnownSymbol p) => HasProp (Always (Eventually p)) where
--   collectPropInfo = Map.singleton (symbolVal (Proxy @p)) makePropInfoInfinitelyOften
--   {-# INLINE collectPropInfo #-}

-- -- | @since 0.1.0.0
-- instance (KnownSymbol p) => HasProp (Eventually (Always p)) where
--   collectPropInfo = Map.singleton (symbolVal (Proxy @p)) makePropInfoStaysAs
--   {-# INLINE collectPropInfo #-}

-- -- | @since 0.1.0.0
-- instance (HasProp p, HasProp q) => HasProp (p /\ q) where
--   collectPropInfo = Map.unionWith (<>) (collectPropInfo @p) (collectPropInfo @q)
--   {-# INLINE CONLIKE collectPropInfo #-}

-- -- ---------------------------------------------------------------------------------------------------------------------

-- data PropInfo = PropInfo
--   { propInfoIsAlways :: Bool
--   , propInfoIsEventually :: Bool
--   , propInfoLeadsTo :: Set String
--   , propInfoIsInfinitelyOften :: Bool
--   , propInfoIsStaysAs :: Bool
--   }
--   deriving (Show)

-- -- | @since 0.1.0.0
-- instance Semigroup PropInfo where
--   PropInfo g1 f1 lt1 gf1 fg1 <> PropInfo g2 f2 lt2 gf2 fg2 =
--     PropInfo (g1 || g2) (f1 || f2) (Set.union lt1 lt2) (gf1 || gf2) (fg1 || fg2)
--   {-# INLINE (<>) #-}

-- -- | @since 0.1.0.0
-- instance Monoid PropInfo where
--   mempty = PropInfo False False Set.empty False False
--   {-# INLINE CONLIKE mempty #-}

-- makePropInfoAlways :: PropInfo
-- makePropInfoAlways = PropInfo True False Set.empty False False
-- {-# INLINE CONLIKE makePropInfoAlways #-}

-- makePropInfoEventually :: PropInfo
-- makePropInfoEventually = PropInfo False True Set.empty False False
-- {-# INLINE CONLIKE makePropInfoEventually #-}

-- makePropInfoLeadsTo :: Set String -> PropInfo
-- makePropInfoLeadsTo actions = PropInfo False False actions False False
-- {-# INLINE CONLIKE makePropInfoLeadsTo #-}

-- makePropInfoInfinitelyOften :: PropInfo
-- makePropInfoInfinitelyOften = PropInfo False False Set.empty True False
-- {-# INLINE CONLIKE makePropInfoInfinitelyOften #-}

-- makePropInfoStaysAs :: PropInfo
-- makePropInfoStaysAs = PropInfo False False Set.empty False True
-- {-# INLINE CONLIKE makePropInfoStaysAs #-}
