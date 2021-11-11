{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeFamilies #-}

-- |
--
-- @since 0.1.0.0
module Data.Functor.Temporal.Tree
  ( -- * Tree Transformer
    TreeF (TreeF),
    pattern (:&:),
    nodeF,
    subtreesF,

    -- * Forest Transformer
    ForestF (ForestF),
    forestF,

    -- * Trees
    Tree (MkTree),
    pattern Tree,
    pattern Leaf,
    treeF,
    node,
    subtrees,

    -- * Forests
    Forest,
    pattern Forest,
    getForest,
  )
where

import Control.Monad.Zip
import Data.Bifunctor
import Data.Functor.Foldable
import Data.Kind

-- ---------------------------------------------------------------------------------------------------------------------

infixr 5 :&:

type TreeF :: (Type -> Type) -> Type -> Type -> Type
data TreeF m a b = TreeF
  { nodeF :: !a
  , subtreesF :: ForestF m b
  }
  deriving (Functor)

-- | @since 0.1.0.0
instance Functor m => Bifunctor (TreeF m) where
  bimap f g (TreeF x xs) = TreeF (f x) (fmap g xs)
  {-# INLINE bimap #-}

-- | Infix pattern synonym for the 'TreeF' constructor.
--
-- @since 0.1.0.0
pattern (:&:) :: a -> m b -> TreeF m a b
pattern x :&: xs = TreeF x (ForestF xs)

{-# COMPLETE (:&:) #-}

-- ---------------------------------------------------------------------------------------------------------------------

-- | 'Forest', transformer for a forest of 'TreeF'.
--
-- @since 0.1.0.0
newtype ForestF :: (Type -> Type) -> Type -> Type where
  ForestF :: {forestF :: m b} -> ForestF m b
  deriving (Functor, Applicative, Monad)

-- | Forests, lists of trees.
--
-- @since 0.1.0.0
type Forest :: Type -> Type
newtype Forest :: Type -> Type where
  MkForest :: {getForestF :: ForestF [] (Tree a)} -> Forest a
  deriving (Functor)

pattern Forest :: [Tree a] -> Forest a
pattern Forest {getForest} = MkForest (ForestF getForest)
{-# COMPLETE Forest #-}

-- | @since 0.1.0.0
instance Foldable Forest where
  foldMap f (Forest ts) = foldMap (foldMap f) ts
  {-# INLINE foldMap #-}

-- | @since 0.1.0.0
instance Show a => Show (Forest a) where
  show (Forest ts) = show ts

-- ---------------------------------------------------------------------------------------------------------------------

-- | Rose trees, 'TreeF' transformed over @[]@.
--
-- @since 0.1.0.0
newtype Tree :: Type -> Type where
  MkTree :: {treeF :: TreeF [] a (Tree a)} -> Tree a

-- | Pattern synonym for constructing 'Tree's.
--
-- @since 0.1.0.0
pattern Tree :: a -> [Tree a] -> Tree a
pattern Tree {node, subtrees} = MkTree (TreeF node (ForestF subtrees))

{-# COMPLETE Tree #-}

-- | Pattern synonym for singleton 'Tree's.
--
-- @since 0.1.0.0
pattern Leaf :: a -> Tree a
pattern Leaf x = Tree x []

{-# COMPLETE Leaf, Tree #-}

-- | @since 0.1.0.0
instance Functor Tree where
  fmap f (Tree x xs) = Tree (f x) (map (fmap f) xs)

-- | @since 0.1.0.0
instance Applicative Tree where
  pure x = Tree x mempty
  {-# INLINE pure #-}

  Tree f fs <*> Tree x xs = Tree (f x) (map (f <$>) xs ++ map (<*> Tree x xs) fs)
  {-# INLINE (<*>) #-}

-- | @since 0.1.0.0
instance Monad Tree where
  Tree x xs >>= f = case f x of
    Tree y ys -> Tree y (ys ++ map (>>= f) xs)
  {-# INLINE (>>=) #-}

-- | @since 0.1.0.0
instance MonadZip Tree where
  mzipWith f (Tree x xs) (Tree y ys) = Tree (f x y) (zipWith (mzipWith f) xs ys)
  {-# INLINE mzipWith #-}

-- | @since 0.1.0.0
type instance Base (Tree a) = TreeF [] a

-- | @since 0.1.0.0
instance Recursive (Tree a) where
  project (Tree x xs) = x :&: xs
  {-# INLINE project #-}

-- | @since 0.1.0.0
instance Corecursive (Tree a) where
  embed (x :&: xs) = Tree x xs
  {-# INLINE embed #-}

-- | @since 0.1.0.0
instance Foldable Tree where
  foldMap f (Tree x ts) = f x <> foldMap (foldMap f) ts

-- | @since 0.1.0.0
instance Show a => Show (Tree a) where
  show (Tree x xs) = show x ++ " :& " ++ show xs
