-- |
--
-- @since 0.1.0.0
module Data.Functor.Temporal.Core
  ( -- * CoreM
    CoreM (CoreM),
    coreM,

    -- ** CoreM Operations
    imageM,
    preimageM,

    -- * CoreB
    CoreB,

    -- ** CoreB Operations
    searchB,
    forallB,
    existsB,
    unionsB,
  )
where

import Control.Monad (ap)
import Data.Bool (bool)
import Data.Kind (Type)
import Data.Profunctor (Profunctor, dimap, lmap, rmap)

-- ---------------------------------------------------------------------------------------------------------------------

-- | The core temporal (pro)functor.
--
-- The projection @'coreM' \x -> p x@ can be read as "find a x such that p(x) holds."
--
-- @since 0.1.0.0
newtype CoreM :: (Type -> Type) -> Type -> Type -> Type where
  CoreM :: {coreM :: (b -> m a) -> m b} -> CoreM m a b

-- | @since 0.1.0.0
instance Functor m => Functor (CoreM m a) where
  fmap f xs = CoreM \p -> fmap f (coreM xs (p . f))
  {-# INLINE fmap #-}

-- | @since 0.1.0.0
instance Functor m => Profunctor (CoreM m) where
  dimap f g (CoreM k) = CoreM \p ->
    fmap g (k (fmap f . p . g))
  {-# INLINE dimap #-}

  lmap = preimageM
  {-# INLINE lmap #-}

  rmap = imageM
  {-# INLINE rmap #-}

-- | @'imageM' f xs@ is a search under the inverse-image of @f@. For @imageM f \x -> p x@ the search produces elements
-- @y@ in @c@ where @p(f(x)) = y@ holds.
--
-- Note: @'imageM' f xs@ is equivalent to @'fmap' f xs@.
--
-- @since 0.1.0.0
imageM :: Functor m => (b -> c) -> CoreM m a b -> CoreM m a c
imageM f xs = CoreM \p -> f <$> coreM xs (p . f)
{-# INLINE imageM #-}

-- | Contramap over the left type parameter.
--
-- Note: @'preiamgeM' f xs@ is equivalent to @'lmap' f xs@.
--
-- @since 0.1.0.0
preimageM :: Functor m => (b -> a) -> CoreM m a c -> CoreM m b c
preimageM f xs = CoreM \p -> coreM xs (fmap f . p)
{-# INLINE preimageM #-}

-- ---------------------------------------------------------------------------------------------------------------------

-- | Synonym of 'CoreM' specialized to boolean-valued functions (predicates).
--
-- @since 0.1.0.0
type CoreB :: (Type -> Type) -> Type -> Type

type CoreB m = CoreM m Bool

-- | @since 0.1.0.0
instance Monad m => Applicative (CoreB m) where
  pure x = CoreM \_ -> pure x
  {-# INLINE pure #-}

  (<*>) = ap
  {-# INLINE (<*>) #-}

-- | @since 0.1.0.0
instance Monad m => Monad (CoreB m) where
  m >>= f = unionsB (fmap f m)
  {-# INLINE (>>=) #-}

searchB :: Monad m => CoreB m a -> (a -> m Bool) -> m (Maybe a)
searchB xs p = do
  x <- coreM xs p
  bool Nothing (Just x) <$> p x
{-# INLINE searchB #-}

forallB :: Monad m => CoreB m a -> (a -> m Bool) -> m Bool
forallB xs p = fmap not (existsB xs (fmap not . p))
{-# INLINE forallB #-}

existsB :: Monad m => CoreB m a -> (a -> m Bool) -> m Bool
existsB xs p = coreM xs p >>= p
{-# INLINE existsB #-}

unionsB :: Monad m => CoreB m (CoreB m b) -> CoreB m b
unionsB xss = CoreM \p -> coreM xss (`existsB` p) >>= \xs -> coreM xs p
{-# INLINE unionsB #-}
