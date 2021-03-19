-- | Inductive classes operating on 'HListT'.
--
-- @since 0.1.0.0
module Data.Type.HList.Class
  ( (:<>) (..),
  )
where

import Data.Type.HList.Internal (HList, HListT ((:.:)), pattern (:<:))

-- -----------------------------------------------------------------------------

-- | The '(:<>)' constraint can be read as "x occurs in xs" for some @x :<> xs@,
-- it enforces that at least one element of the type @x@ must occur in the
-- 'HListT' signature @xs@.
--
-- @since 0.1.0.0
class x :<> xs where
  -- | 'project' lets elements of the given type @x@ to be "projected" from a
  -- @xs@. While @xs@ is typically an 'HListT' it is intentionally left
  -- ambiguous so that wrappers over 'HListT' can be made into '(:<>)' instances.
  --
  -- [Example]
  --
  -- >>> let things :: HList '[Int, String]
  -- >>>     things = 5 :<: "teapot" :<: HNil
  -- >>>
  -- >>> project things :: String
  -- >>> "teapot"
  --
  -- [Caveats]
  --
  -- '(:<>)' instances are defined inductively, so if two nominally equal types
  -- occur in @xs@ the element of type @x@ nearest to the head of the 'HListT'
  -- will /always/ be projected. For instance if a new type of 'String' was
  -- added to `things`.
  --
  -- >>> let xs :: HList '[String, Int, String]
  -- >>>     xs = "dragon" :<: 5 :<: "teapot" :<: HNil
  -- >>>
  -- >>> project xs :: String
  -- >>> "dragon"
  --
  -- @since 0.1.0.0
  project :: xs -> x

-- | @since 0.1.0.0
instance {-# OVERLAPS #-} x :<> HList (x ': xs) where
  project (x :<: _) = x
  {-# INLINE project #-}

-- | @since 0.1.0.0
instance x :<> HList xs => x :<> HList (y ': xs) where
  project (_ :<: xs) = project xs
  {-# INLINE project #-}

-- | @since 0.1.0.0
instance {-# OVERLAPS #-} m x :<> HListT m (x ': xs) where
  project (x :.: _) = x
  {-# INLINE project #-}

-- | @since 0.1.0.0
instance m x :<> HListT m xs => m x :<> HListT m (y ': xs) where
  project (_ :.: xs) = project xs
  {-# INLINE project #-}
