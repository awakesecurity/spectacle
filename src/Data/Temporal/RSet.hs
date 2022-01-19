{-# LANGUAGE Arrows #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

-- |
--
-- @since 0.1.0.0
module Data.Temporal.RSet
  ( -- * Reactive Types
    RSet (RSet, getRSet),
    fromAction,
    intoTime,
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
import Lens.Micro.Mtl (view)

import Data.Temporal.Time (Interval (Interval), Time, pattern Inf, pattern Time)
import Data.Type.Rec (Rec)
import Data.World (World, worldValues)
import Language.Spectacle.AST.Action (Action, runAction)

-- ---------------------------------------------------------------------------------------------------------------------

newtype RSet :: Type -> Type -> Type where
  RSet :: {getRSet :: a -> b} -> RSet a b
  deriving (Functor, Applicative)
  deriving (Arrow, ArrowChoice, Category, Profunctor, Strong) via (->)

fromAction :: Hashable (Rec ctxt) => Action ctxt Bool -> RSet (World ctxt) (Set (World ctxt))
fromAction act = RSet ((`runAction` act) . view worldValues)

intoTime :: Eq m => (a -> m) -> RSet a m -> RSet a (Time m)
intoTime sing ty = proc dom -> do
  codom <- ty -< dom
  if sing dom == codom
    then returnA -< Inf
    else returnA -< Time codom

intoInterval :: RSet a (Time b) -> RSet a (Interval a b)
intoInterval ty = proc dom -> do
  codom <- ty -< dom
  returnA -< Interval dom codom

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
