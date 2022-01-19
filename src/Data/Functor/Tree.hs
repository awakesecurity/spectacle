{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeFamilies #-}

-- |
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
    breadthR,
    mergeWith,
    mergeS,
    levels,
  )
where

import Control.Applicative (Applicative (liftA2))

import Control.Applicative.Queue (later, now, runQueue, runQueueReverse)
import Data.Tree (Tree (Node), flatten)

infixr 5 :-

-- ---------------------------------------------------------------------------------------------------------------------

pattern (:-) :: a -> [Tree a] -> Tree a
pattern x :- xs = Node x xs

pattern Leaf :: a -> Tree a
pattern Leaf x = x :- []

{-# COMPLETE (:-) #-}

rootOf :: Tree a -> a
rootOf (x :- _) = x

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

breadth :: Applicative f => (a -> f b) -> Tree a -> f (Tree b)
breadth f = runQueue . go
  where
    go (t :- ts) = liftA2 (:-) (now (f t)) (later (traverse go ts))

breadthR :: Applicative f => (a -> f b) -> Tree a -> f (Tree b)
breadthR f = runQueueReverse . go
  where
    go (t :- ts) = liftA2 (:-) (now (f t)) (later (traverse go ts))

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

mergeS :: Semigroup a => Tree a -> Tree a -> Tree a
mergeS = mergeWith (<>)
