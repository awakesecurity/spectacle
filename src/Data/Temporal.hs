{-# LANGUAGE TupleSections #-}

-- |
--
-- @since 0.1.0.0
module Data.Temporal
  (
  )
where

import Control.Applicative
import Control.Comonad.Cofree
import Data.Set (Set)
import qualified Data.Set as Set

import Data.Temporal.RSet
import Data.Temporal.Time

-- ---------------------------------------------------------------------------------------------------------------------

-- -- | 'RSet' is a reactive type. Practically this is @World ctxt -> Set (World ctxt)@ but is intentionally made
-- -- polymorphic to avoid handling 'Set' and its terminal 'Ord' constraint directly.
-- --
-- -- The TLA analog to reactive types are predicates since for every action.
-- --
-- -- @since 0.1.0.0
-- newtype RSet :: Type -> Type -> Type where
--   RSet :: {unboxRSet :: a -> b} -> RSet a b
--   deriving (Functor, Applicative)
--   deriving (Arrow, ArrowChoice, Category, Profunctor, Strong) via (->)

-- -- | @since 0.1.0.0
-- deriving via ((->) m) instance Monoid m => Comonad (RSet m)

-- -- | @since 0.1.0.0
-- instance SetRep RSet where
--   unbox (Unbox f x) = unboxRSet f x
--   {-# INLINE unbox #-}

-- fromAction :: Hashable (Rec ctxt) => Action ctxt Bool -> RSet (World ctxt) (Set (World ctxt))
-- fromAction action = RSet ((`runAction` action) . view worldValues)
-- {-# INLINE fromAction #-}

-- labelRSet :: i -> RSet a b -> RSet a (i, b)
-- labelRSet ix = fmap (ix,)
-- {-# INLINE labelRSet #-}

-- intoTime :: Eq a => RSet a (Set a) -> RSet a (Time (Set a))
-- intoTime ty = proc dom -> do
--   codom <- ty -< dom
--   if Set.singleton dom == codom
--     then returnA -< Inf
--     else returnA -< Time codom

-- cospans :: (Foldable f, Monoid b) => f (RSet a b) -> RSet a b
-- cospans fs = RSet \xs -> foldMap (($ xs) . unboxRSet) fs

-- -- ---------------------------------------------------------------------------------------------------------------------

-- newtype RPred :: Type -> Type -> Type where
--   RPred :: {getRPred :: Interval a (Set (i, Set a))} -> RPred i a

-- -- ---------------------------------------------------------------------------------------------------------------------

-- newtype Global :: (Type -> Type) -> Type -> Type where
--   GL :: {getGlobal :: Cofree f a} -> Global f a
--   deriving (Functor, Applicative, Monad)

-- -- | @since 0.1.0.0
-- instance Functor f => Comonad (Global f) where
--   extract = extract . getGlobal
--   {-# INLINE extract #-}

--   extend f = GL . extend (f . GL) . getGlobal
--   {-# INLINE extend #-}

-- splitGL :: Functor f => Global f a -> (a, Global f a)
-- splitGL gl = (extract gl, gl)
-- {-# INLINE splitGL #-}

-- -- ---------------------------------------------------------------------------------------------------------------------

-- newtype Future :: (Type -> Type) -> Type -> Type where
--   Future :: {getFuture :: Free f a} -> Future f a

-- -- ---------------------------------------------------------------------------------------------------------------------

-- -- | Exponential objects wrapped in 'Unbox' can be unpack directly into their 'Set' representations.
-- --
-- -- Just as we can naturally transform the Traced comonad into a Store by pairing the inner function with a point in the
-- -- codomain, we can lower 'RSet' and 'RMor' into their 'Set' representations by chosing a point in time to observe their
-- -- inhabitants at.
-- --
-- -- @since 0.1.0.0
-- data Unbox :: (Type -> Type -> Type) -> Type -> Type -> Type where
--   Unbox :: p a b -> a -> Unbox p a b
--   deriving (Functor)

-- -- | @since 0.1.0.0
-- instance (Arrow p, SetRep p, Functor (p a)) => Comonad (Unbox p a) where
--   extract = unbox
--   {-# INLINE extract #-}

--   extend f (Unbox p x) = Unbox (p >>> arr (const (Unbox p x)) >>^ f) x
--   {-# INLINE extend #-}

--   duplicate (Unbox p x) = Unbox (arr (const (Unbox p x))) x
--   {-# INLINE duplicate #-}

-- -- | Natural transformations over 'Unbox'.
-- --
-- -- @since 0.1.0.0
-- natUnbox :: (forall x y. p x y -> q x y) -> Unbox p a b -> Unbox q a b
-- natUnbox nt (Unbox p x) = Unbox (nt p) x

-- unboxPrecompose :: Arrow p => (a -> b) -> (b -> a) -> Unbox p a c -> Unbox p b c
-- unboxPrecompose f g (Unbox p x) = Unbox (g ^>> p) (f x)

-- -- | Inhabitance predicate for some 'Foldable' structure @f@. Canonical example is when @p@ is 'RSet' and @f@ is 'Set',
-- -- then
-- --
-- -- @
-- -- inhabited :: Unbox RSet a (Set a) ~ (RSet a (Set a), a) -> (RSet a (Set a), a) -> Bool
-- -- inhabited (RSet f, x) = null (f x)
-- -- @
-- --
-- -- @since 0.1.0.0
-- inhabited :: (SetRep p, Foldable f) => Unbox p a (f b) -> Bool
-- inhabited = null . unbox
-- {-# INLINE inhabited #-}

-- repackage :: Unbox p a b -> a -> Unbox p a b
-- repackage (Unbox f _) = Unbox f
-- {-# INLINE repackage #-}

-- -- | Instances of 'SetRep' are profunctors modeling exponential objects with evaluation mapping 'unbox'. Any instance of
-- -- 'Sieve' should have a free instance of 'SetRep', and ideally 'SetRep' would just be a sieve into 'Set', but 'Set' is
-- -- not a functor.
-- --
-- -- @since 0.1.0.0
-- class Profunctor p => SetRep p where
--   unbox :: Unbox p a b -> b

-- unboxCarry :: SetRep p => Unbox p a b -> (a, b)
-- unboxCarry (Unbox f x) = (x, unbox (Unbox f x))
-- {-# INLINE unboxCarry #-}
