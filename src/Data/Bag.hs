-- |
--
-- @since 0.1.0.0
module Data.Bag
  ( Bag (None, Some),
    empty,
    cons,
    singleton,
    zipBagWith,
  )
where

import Data.Kind (Type)

import Data.Node (Node (Leaf, (:*:)), zipNodeWith)

-- ---------------------------------------------------------------------------------------------------------------------

data Bag :: Type -> Type where
  None :: Bag a
  Some :: Node a -> Bag a
  deriving (Show)

empty :: Bag a
empty = None
{-# INLINE CONLIKE empty #-}

cons :: a -> Bag a -> Bag a
cons x None = Some (Leaf x)
cons x (Some xs) = Some (Leaf x :*: xs)
{-# INLINE cons #-}

singleton :: a -> Bag a
singleton = Some . Leaf
{-# INLINE CONLIKE singleton #-}

zipBagWith :: (a -> b -> c) -> Bag a -> Bag b -> Bag c
zipBagWith _ None _ = None
zipBagWith _ _ None = None
zipBagWith op (Some xs) (Some ys) = Some (zipNodeWith op xs ys)

-- | @since 0.1.0.0
instance Functor Bag where
  fmap _ None = None
  fmap f (Some xs) = Some (fmap f xs)
  {-# INLINE fmap #-}

-- | @since 0.1.0.0
instance Applicative Bag where
  pure = Some . Leaf
  {-# INLINE pure #-}

  None <*> _ = None
  _ <*> None = None
  Some fs <*> Some xs = Some (fs <*> xs)
  {-# INLINE (<*>) #-}

-- | @since 0.1.0.0
instance Semigroup (Bag a) where
  None <> ys = ys
  xs <> None = xs
  Some xs <> Some ys = Some (xs <> ys)
  {-# INLINE (<>) #-}

-- | @since 0.1.0.0
instance Monoid (Bag a) where
  mempty = None
  {-# INLINE CONLIKE mempty #-}

-- | @since 0.1.0.0
instance Foldable Bag where
  foldMap _ None = mempty
  foldMap f (Some xs) = foldMap f xs
  {-# INLINE foldMap #-}

  foldr _ nil None = nil
  foldr cons nil (Some xs) = foldr cons nil xs
  {-# INLINE foldr #-}

-- | @since 0.1.0.0
instance Traversable Bag where
  traverse _ None = pure None
  traverse f (Some xs) = fmap Some (traverse f xs)
  {-# INLINE traverse #-}
