{-# LANGUAGE AllowAmbiguousTypes #-}

-- | Type-level induction principles.
--
-- @since 0.1.0.0
module Data.Type.Induction
  ( WInduct (..),
    WInductMaybe (..),
  )
where

import Control.Applicative (liftA2)
import Data.Kind (Constraint, Type)

import Data.Type.HList (HList, HListT)

-- -----------------------------------------------------------------------------

-- | Kind polymorphic, W-type induction. The type variable @x@ is the base case
-- and @xs@ is the W-type being inducted on.
--
-- >>> reifyIx @_ @[_] @40 @[10, 20, 30, 40, 50]
-- >>> 3
--
-- This class is made kind-polymorphic so that all W-types and or wrappers
-- indexed by W-types can be made into instances, e.g.
--
-- >>> reifyIx @_ @_ @Int @(HList '[String, Maybe Bool, Int, Double])
-- >>> 2
--
-- The instance for the base case should be marked with an "overlaps" pragma
-- otherwise, GHC won't prioritize it over the instance for inductive step. See
-- the 'WInduct' instance for type-level lists as an example.
--
-- @since 0.1.0.0
type WInduct :: i -> j -> Constraint
class WInduct x xs where
  -- | Reify the number of inductive steps taken to reach the base case as a
  -- 'Word'.
  --
  -- @since 0.1.0.0
  reifyIx :: Word

-- | @since 0.1.0.0
instance {-# OVERLAPS #-} WInduct x (x ': xs) where
  reifyIx = 0
  {-# INLINE CONLIKE reifyIx #-}

-- | @since 0.1.0.0
instance WInduct x xs => WInduct x (y ': xs) where
  reifyIx = 1 + reifyIx @_ @[_] @x @xs
  {-# INLINE CONLIKE reifyIx #-}

-- | @since 0.1.0.0
instance WInduct @Type x xs => WInduct (m x) (HListT m xs) where
  reifyIx = reifyIx @_ @[_] @x @xs
  {-# INLINE CONLIKE reifyIx #-}

-- | @since 0.1.0.0
instance WInduct @Type x xs => WInduct x (HList xs) where
  reifyIx = reifyIx @_ @[_] @x @xs
  {-# INLINE CONLIKE reifyIx #-}

-- | Functions identitically to 'WInduct' except it does not enforce the
-- constraint that @x@ must eventually occur in @xs@.
--
-- @since 0.1.0.0
type WInductMaybe :: i -> j -> Constraint
class WInductMaybe x xs where
  -- | 'reifyMaybeIx' is 'reifyIx' with failure.
  --
  -- @
  -- 'Just' 'reifyIx' == 'reifyMaybeIx'
  -- @
  --
  -- @since 0.1.0.0
  reifyMaybeIx :: Maybe Word

-- | @since 0.1.0.0
instance WInductMaybe x '[] where
  reifyMaybeIx = Nothing
  {-# INLINE CONLIKE reifyMaybeIx #-}

-- | @since 0.1.0.0
instance {-# OVERLAPS #-} WInductMaybe x (x ': xs) where
  reifyMaybeIx = Just 0
  {-# INLINE CONLIKE reifyMaybeIx #-}

-- | @since 0.1.0.0
instance WInductMaybe x xs => WInductMaybe x (y ': xs) where
  reifyMaybeIx = liftA2 (+) (Just 1) (reifyMaybeIx @_ @[_] @x @xs)
  {-# INLINE CONLIKE reifyMaybeIx #-}

-- | @since 0.1.0.0
instance WInductMaybe @Type x xs => WInductMaybe (m x) (HListT m xs) where
  reifyMaybeIx = reifyMaybeIx @_ @[_] @x @xs
  {-# INLINE CONLIKE reifyMaybeIx #-}

-- | @since 0.1.0.0
instance WInductMaybe @Type x xs => WInductMaybe x (HList xs) where
  reifyMaybeIx = reifyMaybeIx @_ @[_] @x @xs
  {-# INLINE CONLIKE reifyMaybeIx #-}
