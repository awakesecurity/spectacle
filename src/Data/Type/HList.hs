-- | The 'HListT' type represents a finite list of heterogeneously typed
-- elements. It is similar to a normal list type but is indexed with a
-- type-level list the types of elements at that position in the 'HListT':
--
-- @
-- things :: HListT m (String   ':  Double ':  Int ': ':  [Int]     ': [])
--                       |           |         |           |            |
-- things =           "teapot" :.:  3.14  :.: 100 :.: [1, 2, 3, 4] :.: HNil
-- @
--
-- [Caveats]
--
-- Since 'HListT' is indexed by a type-level list many of the typical list-like
-- operations on containers are impossible to define for 'HListT'. Furthermore,
-- 'HListT' cannot be converted to any other container which is not isomorphic
-- to 'HListT' such as lists and vice versa.
--
-- The scope of their application is very narrow and should only be used as a
-- last option.
--
-- @since 0.1.0.0
module Data.Type.HList
  ( -- * Heterogenous Lists
    HList,
    pattern (:<:),
    HListT (..),

    -- * Classes
    (:<>) (..),

    -- * Operations on HLists
    inject,
    surject,
    hoist,
    head,
    zipWith,
    tail,
    length,
    foldl,
    foldMap,

    -- * Operations on HListTs
    injectM,
    surjectM,
    zipWithM,
    hoistM,
    foldlM,
    foldMapM,
  )
where

import Control.Applicative (liftA2)
import Prelude
  ( Applicative (pure),
    Int,
    Monad (return),
    Monoid (mempty),
    Semigroup ((<>)),
    (+),
  )

import Data.Type.HList.Class ((:<>) (project))
import Data.Type.HList.Internal
  ( HListT (HNil, (:.:)),
    pattern (:<:),
    type HList,
  )

-- -----------------------------------------------------------------------------

-- | \(\mathcal{O}(n)\). Inverse of 'surject'. Inject each element of a 'HList'
-- into a structure, i.e.
--
-- @
-- toSingletons :: HList '[Int, String] -> HListT [] '[Int, String]
-- toSingletons xs = inject (\x -> [x]) x
-- @
--
-- @since 0.1.0.0
inject :: (forall a. a -> m a) -> HList xs -> HListT m xs
inject _ HNil = HNil
inject f (x :<: xs) = f x :.: inject f xs

-- | \(\mathcal{O}(n)\). Inverse of 'inject'. Evaluate the structures of an
-- 'HListT' into a 'HList', i.e.
--
-- @
-- execWriters :: HListT Writer '[[Int], String] -> HList '[[Int], String]
-- execWriters xs = surject execWriter xs
-- @
--
-- @since 0.1.0.0
surject :: (forall a. m a -> a) -> HListT m xs -> HList xs
surject _ HNil = HNil
surject f (x :.: xs) = f x :<: surject f xs

-- | Hoist a action out of an 'HListT' with the provided natural transformation.
--
-- @since 0.1.0.0
hoist ::
  Applicative g =>
  (forall a. f a -> g a) ->
  HListT f xs ->
  HListT g xs
hoist _ HNil = HNil
hoist f (x :.: xs) = f x :.: hoist f xs

-- | \(\mathcal{O}(1)\). Extract the first element of a 'HList'.
--
-- @since 0.1.0.0
head :: HList (x ': xs) -> x
head (x :<: _) = x

zipWith ::
  (forall a. f a -> g a -> m a) ->
  HListT f xs ->
  HListT g xs ->
  HListT m xs
zipWith _ HNil HNil = HNil
zipWith f (x :.: xs) (y :.: ys) = f x y :.: zipWith f xs ys

-- | \(\mathcal{O}(1)\). Extract the sublist from the 'HList' provided.
--
-- @since 0.1.0.0
tail :: HList (x ': xs) -> HList xs
tail (_ :<: xs) = xs

-- | \(\mathcal{O}(n)\). Returns the size/length of a 'HList'.
--
-- @since 0.1.0.0
length :: HList xs -> Int
length = foldl (\c _ -> c + 1) 0

-- | \(\mathcal{O}(n)\). Left-associative (left to right) fold a 'HList'.
--
-- @since 0.1.0.0
foldl :: forall b xs. (forall a. b -> a -> b) -> b -> HList xs -> b
foldl _ c HNil = c
foldl f c (x :<: xs) = foldl f (f c x) xs

-- | \(\mathcal{O}(n)\). Map each element of a 'HList' into a 'Monoid' and
-- combine the results.
--
-- @since 0.1.0.0
foldMap :: Monoid b => (forall a. a -> b) -> HList xs -> b
foldMap _ HNil = mempty
foldMap f (x :<: xs) = f x <> foldMap f xs

-- -----------------------------------------------------------------------------

-- | \(\mathcal{O}(n)\). Inverse of 'surjectM'. Inject each element of a 'HList'
-- into a monadic structure, i.e.
--
-- @
-- toMVars :: HList '[Int, String] -> IO (HListT MVar '[Int, String])
-- toMVars xs = injectM newMVar x
-- @
--
-- @since 0.1.0.0
injectM :: Applicative f => (forall a. a -> f (g a)) -> HList xs -> f (HListT g xs)
injectM _ HNil = pure HNil
injectM f (x :<: xs) = liftA2 (:.:) (f x) (injectM f xs)

-- | \(\mathcal{O}(n)\). Inverse of 'injectM'. Evaluates the structures in a
-- 'HListT' into a 'HList' in an monad(or applicative functor) @f@.
--
-- @
-- toMVars :: HListT MVar '[Int, String] -> IO (HList '[Int, String])
-- toMVars xs = surjectM readMVar x
-- @
--
-- @since 0.1.0.0
surjectM :: Applicative f => (forall a. g a -> f a) -> HListT g xs -> f (HList xs)
surjectM _ HNil = pure HNil
surjectM f (x :.: xs) = liftA2 (:<:) (f x) (surjectM f xs)

-- | Construct a new 'HListT' action by zipping two 'HListT' together.
--
-- @since 0.1.0.0
zipWithM ::
  Applicative m =>
  (forall a. f a -> g a -> m (h a)) ->
  HListT f xs ->
  HListT g xs ->
  m (HListT h xs)
zipWithM _ HNil HNil = pure HNil
zipWithM f (x :.: xs) (y :.: ys) = liftA2 (:.:) (f x y) (zipWithM f xs ys)

-- | Hoist a action out of an 'HListT' with the provided natural transformation
-- into a new action.
--
-- @since 0.1.0.0
hoistM ::
  Applicative m =>
  (forall a. f a -> m (g a)) ->
  HListT f xs ->
  m (HListT g xs)
hoistM _ HNil = pure HNil
hoistM f (x :.: xs) = liftA2 (:.:) (f x) (hoistM f xs)

-- | \(\mathcal{O}(n)\). Left associative (left to right) folding an 'HListT'
-- into the inner structure @m@.
--
-- @since 0.1.0.0
foldlM :: forall m b xs. (forall a. m b -> a -> m b) -> m b -> HListT m xs -> m b
foldlM _ c HNil = c
foldlM f c (x :.: xs) = foldlM f (f c x) xs

-- | \(\mathcal{O}(n)\). Map an action over each element of a 'HList' into a
-- 'Monoid' and combine the results.
--
-- @since 0.1.0.0
foldMapM :: (Monad g, Monoid b) => (forall a. f a -> g b) -> HListT f xs -> g b
foldMapM _ HNil = return mempty
foldMapM f (x :.: xs) = liftA2 (<>) (f x) (foldMapM f xs)
