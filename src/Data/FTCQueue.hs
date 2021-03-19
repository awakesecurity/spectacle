-- | The fast type-aligned queue from Oleg Kiselyov's "Extensible Effects: An
-- Alternative to Monad Transformers." It implements a queue as a binary tree
-- that dequeues from the left-most leaf and queues on right most. All
-- operations average constant time.
--
-- Reference: http://okmij.org/ftp/Haskell/extensible/FTCQueue1.hs
--
-- @since 0.1.0.0
module Data.FTCQueue
  ( -- * FTCQueues
    FTCQueue (..),

    -- * Construction
    singleton,
    pattern (:|>),
    (|><|),

    -- * Deconstruction
    ViewL (..),
    viewl,
  )
where

import Data.Kind (Type)
import GHC.TypeLits (Symbol)

import Language.Spectacle.Type.Rec (Ascribe, Rec)

-- -----------------------------------------------------------------------------

-- | Fast type-aligned queue.
--
-- @since 0.1.0.0
type FTCQueue ::
  ([Ascribe Symbol Type] -> Type -> Type) ->
  [Ascribe Symbol Type] ->
  Type ->
  Type ->
  Type
data FTCQueue m cxt a b where
  Leaf :: (Rec cxt -> a -> m cxt b) -> FTCQueue m cxt a b
  Fork :: FTCQueue m cxt a x -> FTCQueue m cxt x b -> FTCQueue m cxt a b

-- | Create a singleton 'FTCQueue'.
--
-- @since 0.1.0.0
singleton :: (Rec cxt -> a -> m cxt b) -> FTCQueue m cxt a b
singleton = Leaf
{-# INLINE singleton #-}

-- | Pattern synonym for constructing to the end of a 'FTCQueue'.
--
-- @since 0.1.0.0
infixl 5 :|>

pattern (:|>) ::
  FTCQueue m cxt a x ->
  (Rec cxt -> x -> m cxt b) ->
  FTCQueue m cxt a b
pattern queue :|> leaf = Fork queue (Leaf leaf)

-- | Appending two 'FTCQueue's.
--
-- @since 0.1.0.0
infixr 5 |><|

(|><|) :: FTCQueue m cxt a x -> FTCQueue m cxt x b -> FTCQueue m cxt a b
(|><|) = Fork
{-# INLINE CONLIKE (|><|) #-}

-- | Wrapping left-most deconstruction of a 'FTCQueue'.
--
-- @since 0.1.0.0
infixr 5 :<|

data ViewL m cxt a b where
  ViewL :: (Rec cxt -> a -> m cxt b) -> ViewL m cxt a b
  (:<|) :: (Rec cxt -> a -> m cxt x) -> FTCQueue m cxt x b -> ViewL m cxt a b

-- | Deconstructs the left-most leaf of a 'FTCQueue', rotating all other leaves
-- down and to the left.
--
-- @
--         *
--        ╱ ╲         viewl                       *
--    tl *   * tr    =======>  tl' *   :|        ╱ ╲
--      ╱ ╲                                tl'1 *   * tr
-- tl' *   * tl'1
-- @
--
-- @since 0.1.0.0
viewl :: FTCQueue m cxt a b -> ViewL m cxt a b
viewl (Leaf x) = ViewL x
viewl (Fork tl tr) = go tl tr
  where
    go :: FTCQueue m cxt a x -> FTCQueue m cxt x b -> ViewL m cxt a b
    go (Leaf x) tr' = x :<| tr'
    go (Fork tl' tl'1) tr' = go tl' (Fork tl'1 tr')
