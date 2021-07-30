{-# LANGUAGE TupleSections #-}
{-# LANGUAGE UndecidableInstances #-}

module Control.Monad.Heap
  ( HeapT (HeapT, runHeapT),
    searchHeapT,
  )
where

import Control.Applicative (Alternative (empty, (<|>)), liftA2)
import Control.Monad (ap, (<=<))
import Control.Monad.Except (MonadError (catchError, throwError))
import Control.Monad.Morph (embed)
import Control.Monad.Reader (MonadReader (ask, local))
import Control.Monad.State (MonadState (get, put, state))
import Control.Monad.Trans (MonadTrans (lift))
import Control.Monad.Writer.Strict (MonadWriter (listen, pass, writer))
import Data.Bifunctor (Bifunctor (bimap, second))
import Data.Tuple (swap)
import ListT (ListT (ListT))
import qualified ListT

import Control.Monad.Heap.Node (Node (Leaf, (:<)), partitionNodes)

-- ---------------------------------------------------------------------------------------------------------------------

newtype HeapT w m a = HeapT
  {runHeapT :: ListT m (Node w a (HeapT w m a))}

-- | @since 0.1.0.0
instance Functor m => Functor (HeapT w m) where
  fmap f (HeapT xs) = HeapT (bimap f (fmap f) <$> xs)
  {-# INLINE fmap #-}

-- | @since 0.1.0.0
instance Monad m => Applicative (HeapT w m) where
  pure x = HeapT (pure (Leaf x))
  {-# INLINE pure #-}

  (<*>) = ap
  {-# INLINE (<*>) #-}

  -- Explicit implementations for both '(*>)' and '(<*)' are given since the default implementation given by '(<*>)' is
  -- too slow.
  (*>) = (>>)
  {-# INLINE (*>) #-}

  (<*) = flip (>>)
  {-# INLINE (<*) #-}

-- | @since 0.1.0.0
instance Monad m => Monad (HeapT w m) where
  HeapT m >>= f =
    HeapT $
      m >>= \case
        Leaf x -> runHeapT (f x)
        w :< y -> pure (w :< (y >>= f))
  {-# INLINE (>>=) #-}

  x >> y = x >>= const y
  {-# INLINE (>>) #-}

-- | @since 0.1.0.0
instance Monad m => Alternative (HeapT w m) where
  empty = HeapT empty
  {-# INLINE empty #-}

  HeapT x <|> HeapT y = HeapT (x <|> y)
  {-# INLINE (<|>) #-}

-- | @since 0.1.0.0
instance MonadTrans (HeapT w) where
  lift m = HeapT (Leaf <$> lift m)
  {-# INLINE lift #-}

-- | @since 0.1.0.0
instance MonadState s m => MonadState s (HeapT w m) where
  get = lift get
  {-# INLINE get #-}

  put = lift . put
  {-# INLINE put #-}

  state = lift . state
  {-# INLINE state #-}

-- | @since 0.1.0.0
instance MonadReader r m => MonadReader r (HeapT w m) where
  ask = lift ask
  {-# INLINE ask #-}

  local f (HeapT m) = HeapT (embed (lift . local f) m)
  {-# INLINE local #-}

-- | @since 0.1.0.0
instance (Monad m, Monoid w) => MonadWriter w (HeapT w m) where
  writer (x, !w) = HeapT (pure (w :< pure x))
  {-# INLINE writer #-}

  listen = go mempty
    where
      go !w = HeapT . fmap (f w) . runHeapT

      f !w1 (Leaf x) = Leaf (x, w1)
      f !w1 (w2 :< xs) = w2 :< go (w1 <> w2) xs
  {-# INLINE listen #-}

  pass m = HeapT do
    (x, ws) <- flattenHeapT m
    case mergeHeapT (map (swap . bimap pure ($ x)) ws) of
      Nothing -> empty
      Just (w, node) -> return (w :< node)
  {-# INLINE pass #-}

-- | @since 0.1.0.0
instance MonadError e m => MonadError e (HeapT w m) where
  throwError = lift . throwError
  {-# INLINE throwError #-}

  catchError (HeapT xs) k = HeapT (catchError xs (runHeapT . k))
  {-# INLINE catchError #-}

searchHeapT :: (Monad m, Monoid w) => HeapT w m a -> m [(a, w)]
searchHeapT = go mempty <=< popMinT
  where
    go !w1 (xs, Nothing) = pure (map (,w1) xs)
    go !w1 (xs, Just (w2, ys)) = fmap (map (,w1) xs ++) (go (w1 <> w2) =<< popMinT ys)

mergeHeapT :: (Monad m, Semigroup w) => [(w, HeapT w m a)] -> Maybe (w, HeapT w m a)
mergeHeapT [] = Nothing
mergeHeapT [x] = Just x
mergeHeapT [x, y] = Just (x <||> y)
mergeHeapT (x : y : xs) = liftA2 (<||>) (Just (x <||> y)) (mergeHeapT xs)
{-# INLINE mergeHeapT #-}

infixr 5 <||>
(<||>) :: (Monad m, Semigroup w) => (w, HeapT w m a) -> (w, HeapT w m a) -> (w, HeapT w m a)
(x, xs) <||> (y, ys) = (x <> y, xs <|> ys)
{-# INLINE (<||>) #-}

flattenHeapT :: (Monad m, Monoid w) => HeapT w m a -> ListT m (w, [a])
flattenHeapT heap = ListT do
  (xs, ys) <- popMinT heap
  case ys of
    Nothing -> return (Just ((mempty, mempty), empty))
    Just (zs, heap') -> return (Just ((zs, xs), flattenHeapT heap'))
{-# INLINE flattenHeapT #-}

popMinT :: (Monad m, Monoid w) => HeapT w m a -> m ([a], Maybe (w, HeapT w m a))
popMinT (HeapT graph) = second mergeHeapT . partitionNodes <$> ListT.toList graph
{-# INLINE popMinT #-}
