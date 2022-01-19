{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE UndecidableInstances #-}

-- |
--
-- @since 0.1.0.0
module Data.Functor.Temporal.Basic where

import Control.Applicative (Alternative (empty, (<|>)), liftA2)
import Control.Comonad
import Control.Comonad.Store
import Control.Monad (ap, (>=>), forM)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Trans.Class (MonadTrans, lift)
import Data.Functor.Identity (Identity (Identity, runIdentity))
import Data.Kind (Type)
import Data.Set (Set)
import qualified Data.Set as Set
import Control.Comonad.Density

import Control.Monad.Levels (LevelsT, sumFoldable, wrap)
import Control.Monad.State (MonadState (get, put))
import Data.World
import Data.Context
import Data.Functor.Compose
import Data.Profunctor.Sieve
import Data.Foldable
import Data.Traversable

-- ---------------------------------------------------------------------------------------------------------------------

globalM :: (MonadIO m, Show a) => (Time a -> LevelsT m Bool) -> (Time a, Bool) -> CoK (Time a) m (Time a, Bool)
globalM predicate obs@(Infinity, holdsHere) = do
  liftIO (print "global e")
  pure (Infinity, True)
globalM predicate obs@(Time here, holdsHere)
  | Set.null here = do
    lift (liftIO (putStrLn ("G: " ++ show here)))
    if holdsHere
      then pure (Time here, True)
      else error "always error "
  | otherwise = CoK \nextK -> do
    -- liftIO (putStrLn ("G1: " ++ show here))
    there <- nextK obs
    holds <- fmap (holdsHere &&) (predicate there)
    pure obs <|> runCoK (globalM predicate (there, holds)) nextK

futureM :: (MonadIO m, Show a) => (Time a -> LevelsT m Bool) -> (Time a, Bool) -> CoK (Time a) m (Time a, Bool)
futureM predicate obs@(Infinity, holdsHere) = pure (Infinity, True)
futureM predicate obs@(Time here, holdsHere)
  | Set.null here = do
    -- lift (liftIO (putStrLn ("G: " ++ show here)))
    if holdsHere
      then pure (Time here, True)
      else error "future (copying always) error "
  | otherwise = CoK \nextK -> do
    -- liftIO (putStrLn ("G1: " ++ show here))
    there <- nextK obs
    holds <- fmap (holdsHere &&) (predicate there)
    pure obs <|> runCoK (globalM predicate (there, holds)) nextK

-- ---------------------------------------------------------------------------------------------------------------------

data Setw :: Context -> Type -> Type where
  Setw :: (Set (World ctxt) -> a) -> Set (World ctxt) -> Setw ctxt a

-- | @since 0.1.0.0
instance Functor (Setw ctxt) where
  fmap f (Setw g x) = Setw (f . g) x
  {-# INLINE fmap #-}

-- | @since 0.1.0.0
instance Comonad (Setw ctxt) where
  extract (Setw f x) = f x
  {-# INLINE extract #-}

  duplicate (Setw f x) = Setw (Setw f) x
  {-# INLINE duplicate #-}

-- ---------------------------------------------------------------------------------------------------------------------

data Time a = Time (Set a) | Infinity
  deriving (Show)

-- | @since 0.1.0.0
instance Ord a => Semigroup (Time a) where
  Infinity <> xs = xs
  xs <> Infinity = xs
  Time xs <> Time ys = Time (xs <> ys)
  {-# INLINE (<>) #-}

-- | @since 0.1.0.0
instance Ord a => Monoid (Time a) where
  mempty = Infinity

fromTime :: Time a -> Set a
fromTime Infinity = Set.empty
fromTime (Time xs) = xs

-- ---------------------------------------------------------------------------------------------------------------------

newtype K :: Type -> (Type -> Type) -> (Type -> Type) -> Type -> Type where
  K# :: {runK# :: S p w (LevelsT m) a} -> K p w m a
  deriving (Functor, Applicative, Monad, Alternative)

-- | @since 0.1.0.0
instance MonadTrans (K p w) where
  lift = K# . lift . lift
  {-# INLINE lift #-}

-- | @since 0.1.0.0
instance (Comonad w, MonadState s m) => MonadState s (K p w m) where
  get = lift get
  {-# INLINE get #-}

  put = lift . put
  {-# INLINE put #-}

-- | @since 0.1.0.0
instance (Comonad w, MonadIO m) => MonadIO (K p w m) where
  liftIO = lift . liftIO
  {-# INLINE liftIO #-}

pattern K :: (w (a -> LevelsT m p) -> LevelsT m a) -> K p w m a
pattern K {runK} = K# (S runK)
{-# COMPLETE K #-}

type CoK p = K p Identity

pattern CoK :: ((a -> LevelsT m p) -> LevelsT m a) -> CoK p m a
pattern CoK {runCoK} <-
  ((. Identity) . runK -> runCoK)
  where
    CoK f = K (f . runIdentity)
{-# COMPLETE CoK #-}

choiceK :: Set a -> K s w m a
choiceK = K . const . sumFoldable
{-# INLINE choiceK #-}

delay :: Monad m => K s w m a -> K s w m a
delay (K k) = K (wrap . k)

coidealK :: (Monad m, Comonad w) => K s w m a -> K s w m (a, K s w m a)
coidealK k = K \w ->
  let m = w =>> \w' x -> extract w' (x, k)
   in liftA2 (,) (runK k m) (pure k)

coidealsK :: (Monad m, Comonad w) => [K s w m a] -> K s w m [(a, K s w m a)]
coidealsK ks = K \w -> for ks \k -> runK (coidealK k) (extend (\w' x -> extract w' [x]) w)

bigUnionK :: (Monad m, Comonad w) => [K s w m a] -> K s w m [a]
bigUnionK ks = K \w -> do
  let (khere, knext) = (map fst <$> coidealsK ks, map snd <$> coidealsK ks)
  runK khere w <|> runK (knext >>= bigUnionK) w

--   (here, next) <- unzip <$> forM ks \k -> runK (coidealK k) . (w $>) $ \case
--     (x, k) -> do
--       x' <- runK k (extend)
--       _
--   pure here <|> runK (bigUnionK next) w

-- ---------------------------------------------------------------------------------------------------------------------

-- | Search monad with predicate generalized to an arbitrary comonad.
--
-- @since 0.1.0.0
newtype S :: Type -> (Type -> Type) -> (Type -> Type) -> Type -> Type where
  S :: {runS :: w (a -> m p) -> m a} -> S p w m a

type CoS r = S r Identity

pattern CoS :: ((a -> m p) -> m a) -> CoS p m a
pattern CoS {runCoS} <-
  ((. Identity) . runS -> runCoS)
  where
    CoS f = S (f . runIdentity)
{-# COMPLETE CoS #-}

-- | @since 0.1.0.0
instance (Functor w, Functor m) => Functor (S r w m) where
  fmap f (S k) = S (fmap f . k . fmap (. f))
  {-# INLINE fmap #-}

-- | @since 0.1.0.0
instance (Comonad w, Monad m) => Applicative (S r w m) where
  pure = S . const . pure
  {-# INLINE pure #-}

  (<*>) = ap
  {-# INLINE (<*>) #-}

-- | @since 0.1.0.0
instance (Comonad w, Monad m) => Monad (S r w m) where
  S k >>= f = S \s ->
    let h x = runS (f x) s
     in k (extend ((h >=>) . extract) s) >>= h
  {-# INLINE (>>=) #-}

-- | @since 0.1.0.0
instance (Comonad w, Alternative m, Monad m) => Alternative (S r w m) where
  empty = S (const empty)
  {-# INLINE empty #-}

  S f <|> S g = S \s -> f s <|> g s
  {-# INLINE (<|>) #-}

-- | @since 0.1.0.0
instance MonadTrans (S r w) where
  lift m = S (const m)
  {-# INLINE lift #-}

-- | @since 0.1.0.0
instance (Comonad w, MonadState s m) => MonadState s (S p w m) where
  get = lift get
  {-# INLINE get #-}

  put = lift . put
  {-# INLINE put #-}

-- | @since 0.1.0.0
instance (Comonad w, MonadIO m) => MonadIO (S p w m) where
  liftIO = lift . liftIO
  {-# INLINE liftIO #-}
