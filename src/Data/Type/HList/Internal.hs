-- | Internal 'HListT' module defining types and instances.
--
-- @since 0.1.0.0
module Data.Type.HList.Internal
  ( type HList,
    pattern (:<:),
    HListT (..),
  )
where

import Data.Functor.Identity (Identity (Identity))
import Data.Hashable (Hashable, hash, hashWithSalt)

-- -----------------------------------------------------------------------------

-- | A list of heterogenously typed elements.
--
-- @since 0.1.0.0
type HList = HListT Identity

-- | Bidirectional pattern synonym for consing an 'HList' which hides an the
-- inner 'Identity' monad. These two pattern matches are equivalent for 'HList':
--
-- @
-- head :: HList (Int ': xs) -> Int
-- head (Identity x :.: xs) = x
-- @
--
-- @
-- head :: HList (Int ': xs) -> Int
-- head (x :<: xs) = x
-- @
--
-- @since 0.1.0.0
infixr 5 :<:

pattern (:<:) :: () => (x ': ys) ~ xs => x -> HList ys -> HList xs
pattern x :<: xs <-
  Identity x :.: xs
  where
    x :<: xs = Identity x :.: xs
{-# COMPLETE (:<:) #-}

-- | The HList transformer type.
--
-- @since 0.1.0.0
infixr 5 :.:

data HListT m xs where
  HNil :: HListT m '[]
  (:.:) :: m x -> HListT m xs -> HListT m (x ': xs)

-- | @since 0.1.0.0
instance Eq (HListT m '[]) where
  -- Nominal equality
  HNil == HNil = True

-- | @since 0.1.0.0
instance (Eq (m x), Eq (HListT m xs)) => Eq (HListT m (x ': xs)) where
  x :.: xs == y :.: ys = x == y && xs == ys

-- | @since 0.1.0.0
instance Show (HListT m '[]) where
  show HNil = "HNil"

-- | @since 0.1.0.0
instance (Show (m x), Show (HListT m xs)) => Show (HListT m (x ': xs)) where
  show (x :.: xs) = show x ++ " :.: " ++ show xs

-- | @since 0.1.0.0
instance Hashable (HListT m '[]) where
  hashWithSalt salt HNil = hash salt

-- | @since 0.1.0.0
instance
  (Hashable (m x), Hashable (HListT m xs)) =>
  Hashable (HListT m (x ': xs))
  where
  hashWithSalt salt (mx :.: mxs) = hashWithSalt (hashWithSalt salt mx) mxs
