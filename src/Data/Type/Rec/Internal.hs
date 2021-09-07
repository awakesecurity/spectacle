-- |
--
--
module Data.Type.Rec.Internal
  ( -- * Extensible Records Transformer
    RecT(RNilT, RConT),

    -- * Extensible Records
    type Rec,
    pattern RNil,
    pattern RCon
  )
where

import Data.Hashable
import Data.Kind
import Data.Functor.Identity

import Data.Name
import Data.Ascript
import Data.Context

-- ---------------------------------------------------------------------------------------------------------------------

-- |
--
-- @since 0.1.0.0
data RecT :: (Type -> Type) -> Context -> Type where
  RNilT :: RecT f CNil
  RConT :: Name s -> f a -> RecT f ctxt -> RecT f (s # a :< ctxt)

-- | @since 0.1.0.0
instance Show (RecT f CNil) where
  show RNilT = "RNilT"
  {-# INLINE show #-}

-- | @since 0.1.0.0
instance (Show (f a), Show (RecT f ctxt)) => Show (RecT f (s # a :< ctxt)) where
  show (RConT name x r) = show name ++ "=" ++ show x ++ " " ++ show r
  {-# INLINE show #-}

-- | @since 0.1.0.0
instance Eq (RecT f CNil) where
  RNilT == RNilT = True
  {-# INLINE (==) #-}

-- | @since 0.1.0.0
instance (Eq (f a), Eq (RecT f ctxt)) => Eq (RecT f (s # a :< ctxt)) where
  RConT _ x r1 == RConT _ y r2 = x == y && r1 == r2
  {-# INLINE (==) #-}

-- | @since 0.1.0.0
instance Hashable (RecT f CNil) where
  hashWithSalt salt RNilT = salt
  {-# INLINE hashWithSalt #-}

-- | @since 0.1.0.0
instance (Hashable (f x), Hashable (RecT f ctxt)) => Hashable (RecT f (s # x :< ctxt)) where
  hashWithSalt salt (RConT _ x xs) = hashWithSalt (hashWithSalt salt x) xs
  {-# INLINE hashWithSalt #-}

-- | @since 0.1.0.0
instance Ord (RecT f CNil) where
  compare RNilT RNilT = EQ
  {-# INLINE compare #-}

-- | @since 0.1.0.0
instance (Ord (f x), Ord (RecT f ctxt)) => Ord (RecT f (s # x :< ctxt)) where
  compare (RConT _ x xs) (RConT _ y ys) = case compare x y of
    EQ -> compare xs ys
    order -> order
  {-# INLINE compare #-}

-- ---------------------------------------------------------------------------------------------------------------------

-- |
--
-- @since 0.1.0.0
type Rec :: Context -> Type
type Rec ctxt = RecT Identity ctxt

-- |
--
-- @since 0.1.0.0
pattern RNil :: () => CNil ~ ctxt => Rec ctxt
pattern RNil <- RNilT
  where RNil = RNilT
{-# COMPLETE RNil #-}

-- |
--
-- @since 0.1.0.0
pattern RCon :: () => (s # a :< ctxt) ~ ctxt' => Name s -> a -> Rec ctxt -> Rec ctxt'
pattern RCon n x r <-
  RConT n (Identity x) r
  where
    RCon n x r = RConT n (Identity x) r
{-# COMPLETE RCon #-}
