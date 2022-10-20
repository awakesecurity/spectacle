{-# LANGUAGE TypeFamilies #-}

-- | Utilities for manipulating @'Data.Tree.Tree'@s.
--
-- @since 0.1.0.0
module Data.Functor.Tree
  ( -- * Rose Trees
    Tree (Node),
    rootOf,
    leavesOf,

    -- ** Construction
    pattern (:-),
    pattern Leaf,

    -- ** Destruction
    flatten,

    -- ** Maps
    mapWithRoot,

    -- ** Folds
    breadth,
    mergeWith,
    mergeS,
    levels,
  )
where

import Control.Applicative (Applicative (liftA2))

import Control.Applicative.Queue (later, now, runQueue)
import Data.Tree (Tree (Node), flatten)

infixr 5 :-

-- ---------------------------------------------------------------------------------------------------------------------

-- | Produce a @'Tree'@ by taking a value and ascribing a list of @'Tree'@s as its immediate children.
pattern (:-) :: a -> [Tree a] -> Tree a
pattern x :- xs = Node x xs

-- | Produce a singleton @'Tree'@, which is simply a @'Node'@ with no children.
pattern Leaf :: a -> Tree a
pattern Leaf x = x :- []

{-# COMPLETE (:-) #-}

-- | Get the root of a @'Tree'@.
rootOf :: Tree a -> a
rootOf (x :- _) = x

-- | Get the immediate leaves for a @'Tree'@.
leavesOf :: Tree a -> [Tree a]
leavesOf (_ :- xs) = xs

-- | @'mapWithRoot' f g ts@ maps each element with @g@ given the immediate root above the current element and that
-- element. @f@ is used for the root of the tree since it has no element above itself.
--
-- @since 0.1.0.0
mapWithRoot :: (a -> b) -> (a -> a -> b) -> Tree a -> Tree b
mapWithRoot f g (t0 :- ts0) = f t0 :- map (go t0) ts0
  where
    go root (t :- ts) = g root t :- map (go t) ts

-- | Perform breadth-first replacement of values in a tree; this is a variant of @'traverse'@ in disguise.
breadth :: Applicative f => (a -> f b) -> Tree a -> f (Tree b)
breadth f = runQueue . go
  where
    go (t :- ts) = liftA2 (:-) (now (f t)) (later (traverse go ts))

-- | Collapse a @'Tree'@ into a list of lists, where each entry is the level of a tree, from top to bottom.
levels :: Tree a -> [[a]]
levels ts = go ts []
  where
    go (x :- xs) (q : qs) = (x : q) : foldr go qs xs
    go (x :- xs) [] = [x] : foldr go [] xs

-- | 'mergeWith' is a zip over trees that does not truncate larger subtrees.
--
-- @since 0.1.0.0
mergeWith :: (a -> a -> a) -> Tree a -> Tree a -> Tree a
mergeWith f (Leaf x) (y :- ys) = f x y :- ys
mergeWith f (x :- xs) (Leaf y) = f x y :- xs
mergeWith f (x :- xs) (y :- ys) = f x y :- zipWith (mergeWith f) xs ys

-- | Merge two trees together provided the underlying values form a @'Semigroup'@.
mergeS :: Semigroup a => Tree a -> Tree a -> Tree a
mergeS = mergeWith (<>)
