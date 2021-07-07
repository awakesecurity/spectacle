module Control.Monad.Heap.Node
  ( Node (Leaf, (:<)),
    partitionNodes,
  )
where

import Data.Bifunctor (Bifunctor (bimap, first, second))

-- ---------------------------------------------------------------------------------------------------------------------

infixr 5 :<
data Node w a b
  = Leaf {-# UNPACK #-} !a
  | {-# UNPACK #-} !w :< b

-- | @since 0.1.0.0
instance Functor (Node w a) where
  fmap f = \case
    Leaf x -> Leaf x
    x :< y -> x :< f y
  {-# INLINE fmap #-}

-- | @since 0.1.0.0
instance Bifunctor (Node w) where
  bimap f g = \case
    Leaf x -> Leaf (f x)
    w :< x -> w :< g x
  {-# INLINE bimap #-}

  first f = \case
    Leaf x -> Leaf (f x)
    w :< x -> w :< x
  {-# INLINE first #-}

  second f = \case
    Leaf x -> Leaf x
    w :< x -> w :< f x
  {-# INLINE second #-}

partitionNodes :: [Node w a b] -> ([a], [(w, b)])
partitionNodes = foldr f ([], [])
  where
    f (Leaf x) (xs, ys) = (x : xs, ys)
    f (w :< x) (xs, ys) = (xs, (w, x) : ys)
{-# INLINE partitionNodes #-}
