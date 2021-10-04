{-# LANGUAGE GeneralizedNewtypeDeriving #-}

-- |
--
-- @since 0.1.0.0
module Data.Temporal where

import Control.Category (Category)
import Control.Comonad (Comonad (duplicate), extend, extract)
import Control.Comonad.Cofree (Cofree)
import Data.Hashable (Hashable)
import Data.Kind (Type)
import Data.Profunctor (Profunctor (dimap), Strong (first'), second')
import Data.Profunctor.Yoneda (Yoneda (Yoneda))
import Data.Set (Set)
import Lens.Micro.Mtl (view)

import Data.Type.Rec (Rec)
import Data.World (World, worldValues)
import Language.Spectacle.AST (Action, runAction)
import Control.Arrow

-- ---------------------------------------------------------------------------------------------------------------------

-- | 'Time' is the disjoint union of a totally ordered @a@ with infinity.
--
-- @since 0.1.0.0
newtype Time :: Type -> Type where
  TimeInf :: {getTime :: Maybe a} -> Time a
  deriving (Eq, Functor, Applicative, Monad)

pattern Time :: a -> Time a
pattern Time x = TimeInf (Just x)

pattern Inf :: Time a
pattern Inf = TimeInf Nothing

{-# COMPLETE Time, Inf #-}

-- | Extracts the inner monoid of 'Time' sending 'Inf' to 'mempty'.
--
-- @since 0.1.0.0
timeExtract :: Monoid m => Time m -> m
timeExtract Inf = mempty
timeExtract (Time ms) = ms
{-# INLINE timeExtract #-}

-- | 'Inf' annihilates.
--
-- @since 0.1.0.0
instance Semigroup a => Semigroup (Time a) where
  Inf <> _ = Inf
  _ <> Inf = Inf
  Time x <> Time y = Time (x <> y)
  {-# INLINE (<>) #-}

-- | @since 0.1.0.0
instance Monoid a => Monoid (Time a) where
  mempty = Time mempty
  {-# INLINE CONLIKE mempty #-}

-- | @since 0.1.0.0
instance Ord a => Ord (Time a) where
  compare Inf (Time _) = GT
  compare (Time _) Inf = LT
  compare Inf Inf = EQ
  compare (Time m) (Time n) = compare m n
  {-# INLINE compare #-}

-- ---------------------------------------------------------------------------------------------------------------------

-- | 'RSet' is a reactive type. Practically this is @World ctxt -> Set (World ctxt)@ but is intentionally made
-- polymorphic to avoid handling 'Set' and its terminal 'Ord' constraint directly.
--
-- The TLA analog to reactive types are predicates since for every action.
--
-- @since 0.1.0.0
newtype RSet :: Type -> Type -> Type where
  RSet :: {unboxRSet :: a -> b} -> RSet a b
  deriving (Functor, Applicative)
  deriving (Arrow, Category, Profunctor, Strong) via (->)

injectAction :: Hashable (Rec ctxt) => Action ctxt Bool -> RSet (World ctxt) (Set (World ctxt))
injectAction action = RSet ((`runAction` action) . view worldValues)

-- | 'RMor' is a morphism in the category of reactive types. If 'RSet's are predicates on states, then 'RMor' are
-- actions between states.
--
-- @since 0.1.0.0
newtype RMor :: Type -> Type -> Type where
  RMor :: {unboxRMor :: forall x y. (x -> a) -> (b -> y) -> RSet x y} -> RMor a b
  deriving (Functor)
  deriving (Category, Profunctor, Strong) via Yoneda RSet

lower :: RMor a b -> RSet a b
lower (RMor k) = k id id
{-# INLINE lower #-}

embed :: RSet a b -> RMor a b
embed rset = RMor \l r -> dimap l r rset
{-# INLINE embed #-}

-- | @since 0.1.0.0
instance Arrow RMor where
  arr f = RMor \l r -> RSet (r . f . l)
  {-# INLINE arr #-}

  first = first'
  {-# INLINE first #-}

  second = second'
  {-# INLINE second #-}

newtype Global :: (Type -> Type) -> Type -> Type where
  GL :: {getGlobal :: Cofree f a} -> Global f a
  deriving (Functor, Applicative)

-- | @since 0.1.0.0
instance Functor f => Comonad (Global f) where
  extract = extract . getGlobal
  {-# INLINE extract #-}

  extend f = GL . extend (f . GL) . getGlobal
  {-# INLINE extend #-}

splitGL :: Functor f => Global f a -> (a, Global f a)
splitGL gl = (extract gl, gl)
{-# INLINE splitGL #-}

-- | Exponential objects wrapped in 'Unbox' can be unpack directly into their 'Set' representations.
--
-- Just as we can naturally transform the Traced comonad into a Store by pairing the inner function with a point in the
-- codomain, we can lower 'RSet' and 'RMor' into their 'Set' representations by chosing a point in time to observe their
-- inhabitants at.
--
-- @since 0.1.0.0
data Unbox :: (Type -> Type -> Type) -> Type -> Type -> Type where
  Unbox :: p a b -> a -> Unbox p a b
  deriving Functor

-- | @since 0.1.0.0
instance (Arrow p, SetRep p, Functor (p a)) => Comonad (Unbox p a) where
  extract = unbox
  {-# INLINE extract #-}

  extend f (Unbox p x) = Unbox (p >>> arr (const (Unbox p x)) >>^ f) x
  {-# INLINE extend #-}

  duplicate (Unbox p x) = Unbox (arr (const (Unbox p x))) x
  {-# INLINE duplicate #-}

-- | Natural transformations over 'Unbox'.
--
-- @since 0.1.0.0
natUnbox :: (forall x y. p x y -> q x y) -> Unbox p a b -> Unbox q a b
natUnbox nt (Unbox p x) = Unbox (nt p) x

-- | Inhabitance predicate for some 'Foldable' structure @f@. Canonical example is when @p@ is 'RSet' and @f@ is 'Set',
-- then
--
-- @
-- inhabited :: Unbox RSet a (Set a) ~ (RSet a (Set a), a) -> (RSet a (Set a), a) -> Bool
-- inhabited (RSet f, x) = null (f x)
-- @
--
-- @since 0.1.0.0
inhabited :: (SetRep p, Foldable f) => Unbox p a (f b) -> Bool
inhabited = null . unbox
{-# INLINE inhabited #-}

repackage :: Unbox p a b -> a -> Unbox p a b
repackage (Unbox f _) = Unbox f
{-# INLINE repackage #-}

-- | Instances of 'SetRep' are profunctors modeling exponential objects with evaluation mapping 'unbox'. Any instance of
-- 'Sieve' should have a free instance of 'SetRep', and ideally 'SetRep' would just be a sieve into 'Set', but 'Set' is
-- not a functor.
--
-- @since 0.1.0.0
class Profunctor p => SetRep p where
  unbox :: Unbox p a b -> b

unboxCarry :: SetRep p => Unbox p a b -> (a, b)
unboxCarry (Unbox f x) = (x, unbox (Unbox f x))
{-# INLINE unboxCarry #-}

-- | @since 0.1.0.0
instance SetRep RSet where
  unbox (Unbox f x) = unboxRSet f x
  {-# INLINE unbox #-}

-- | @since 0.1.0.0
instance SetRep RMor where
  unbox (Unbox f x) = unboxRSet (lower f) x
  {-# INLINE unbox #-}
