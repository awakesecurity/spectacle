{-# LANGUAGE Arrows #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TupleSections #-}

-- |
--
-- @since 0.1.0.0
module Data.Temporal.RSet
  ( -- * Reactive Types
    RSet (RSet, getRSet),
    fromAction,
    -- intoTime,
    intoInterval,

    -- * Lifted Types
    Lift (Lift),
    liftRenew,
    liftEval,
  )
where

import Control.Arrow (Arrow, ArrowChoice, returnA)
import Control.Category (Category)
import Control.Comonad (Comonad (extend, extract))
import Data.Hashable (Hashable)
import Data.Kind (Type)
import Data.Profunctor (Profunctor, Strong)
import Data.Set (Set)
import qualified Data.Set as Set
import Lens.Micro.Mtl (view)

import Data.Temporal.Time (Interval (Interval), pattern Inf, pattern Time)
import Data.Type.Rec (Rec)
import Data.World (World, worldValues)
import Language.Spectacle.AST.Action (Action, runAction)

-- ---------------------------------------------------------------------------------------------------------------------

newtype RSet :: Type -> Type -> Type where
  RSet :: {getRSet :: a -> b} -> RSet a b
  deriving (Functor, Applicative)
  deriving (Arrow, ArrowChoice, Category, Profunctor, Strong) via (->)

-- | @since 0.1.0.0
instance Semigroup b => Semigroup (RSet a b) where
  RSet f <> RSet g = RSet (\x -> f x <> g x)
  {-# INLINE (<>) #-}

-- | @since 0.1.0.0
instance Monoid b => Monoid (RSet a b) where
  mempty = RSet (const mempty)
  {-# INLINE CONLIKE mempty #-}

fromAction :: Hashable (Rec ctxt) => Action ctxt Bool -> RSet (World ctxt) (Set (World ctxt))
fromAction act = RSet ((`runAction` act) . view worldValues)

intoInterval :: Ord a => RSet a (Set (String, Set a)) -> RSet (Set a) (Set (Interval a (String, a)))
intoInterval ty = RSet $ foldMap \x ->
  let theres = foldMap (\(name, ys) -> Set.map (name,) ys) (getRSet ty x)
   in Set.map (Interval x) theres

-- ---------------------------------------------------------------------------------------------------------------------

data Lift :: (Type -> Type -> Type) -> Type -> Type -> Type where
  Lift :: p a b -> a -> Lift p a b
  deriving (Functor)

-- | @since 0.1.0.0
instance Comonad (Lift RSet a) where
  extract (Lift f x) = getRSet f x
  {-# INLINE extract #-}

  extend f (Lift p x) = Lift (RSet (f . Lift p)) x
  {-# INLINE extend #-}

liftRenew :: Lift RSet a b -> a -> Lift RSet a b
liftRenew (Lift f _) = Lift f
{-# INLINE liftRenew #-}

liftEval :: Lift RSet a b -> b
liftEval (Lift (RSet f) x) = f x
{-# INLINE liftEval #-}
