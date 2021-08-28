module Data.Node
  ( Node (Leaf, (:*:)),
  )
where

import Data.Kind (Type)
import GHC.Base (Applicative (liftA2))

-- ---------------------------------------------------------------------------------------------------------------------

infixr 5 :*:

data Node :: Type -> Type where
  Leaf :: !a -> Node a
  (:*:) :: Node a -> Node a -> Node a
  deriving (Show)

-- | @since 0.1.0.0
instance Functor Node where
  fmap f (Leaf x) = Leaf (f x)
  fmap f (xs :*: ys) = fmap f xs :*: fmap f ys
  {-# INLINE fmap #-}

-- | @since 0.1.0.0
instance Applicative Node where
  pure = Leaf
  {-# INLINE CONLIKE pure #-}

  Leaf f <*> xs = fmap f xs
  fs :*: gs <*> xs = (fs <*> xs) :*: (gs <*> xs)
  {-# INLINE (<*>) #-}

-- | @since 0.1.0.0
instance Semigroup (Node a) where
  (<>) = (:*:)
  {-# INLINE CONLIKE (<>) #-}

-- | @since 0.1.0.0
instance Foldable Node where
  foldMap f (Leaf x) = f x
  foldMap f (xs :*: ys) = foldMap f xs <> foldMap f ys
  {-# INLINE foldMap #-}

-- | @since 0.1.0.0
instance Traversable Node where
  traverse f (Leaf x) = fmap Leaf (f x)
  traverse f (xs :*: ys) = liftA2 (:*:) (traverse f xs) (traverse f ys)
  {-# INLINE traverse #-}
