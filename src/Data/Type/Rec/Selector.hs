{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE UndecidableInstances #-}

-- |
module Data.Type.Rec.Selector
  ( -- * Record Selectors
    type (.|),
    HasSel,
    getRec,
    setRec,

    -- ** Transformed
    getRecT,
    setRecT,

    -- * Reflection
    ReflectRow,
    repeatRow,
  )
where

import Data.Functor.Identity (Identity (Identity, runIdentity))
import Data.Kind (Constraint, Type)
import GHC.TypeLits (KnownSymbol, Symbol)

import Data.Ascript (Ascribe, AscriptName, AscriptType, type (#))
import Data.Context (CNil, Context, type (:<))
import Data.Name (Name, inferName)
import Data.Type.Rec.Internal (Rec, RecT (RConT, RNilT))

-- ---------------------------------------------------------------------------------------------------------------------

infixr 3 .|
type (.|) :: Ascribe Symbol Type -> Context -> Constraint
type a .| ctx = HasSel ctx (AscriptName a) (AscriptType a)

setRec :: forall ctx s a. s # a .| ctx => Name s -> a -> Rec ctx -> Rec ctx
setRec name x = setRecT name (Identity x)
{-# INLINE setRec #-}

getRec :: forall ctx s a. s # a .| ctx => Name s -> Rec ctx -> a
getRec n r = runIdentity (getRecT n r)
{-# INLINE getRec #-}

-- |
--
-- @since 0.1.0.0
class HasSel ctx s a | ctx s -> a where
  getRecT :: Name s -> RecT f ctx -> f a

  setRecT :: Name s -> f a -> RecT f ctx -> RecT f ctx

-- | @since 0.1.0.0
instance {-# OVERLAPS #-} HasSel (s # a :< ctx) s a where
  getRecT _ (RConT _ x _) = x
  {-# INLINE getRecT #-}

  setRecT _ x (RConT name _ r) = RConT name x r
  {-# INLINE setRecT #-}

-- | @since 0.1.0.0
instance HasSel ctx s a => HasSel (t # b :< ctx) s a where
  getRecT name (RConT _ _ r) = getRecT name r
  {-# INLINE getRecT #-}

  setRecT name x (RConT name' y r) = RConT name' y (setRecT name x r)
  {-# INLINE setRecT #-}

-- |
--
-- @since 0.1.0.0
class ReflectRow ctx where
  repeatRow :: (forall a. f a) -> RecT f ctx

-- | @since 0.1.0.0
instance ReflectRow CNil where
  repeatRow _ = RNilT
  {-# INLINE repeatRow #-}

-- | @since 0.1.0.0
instance (KnownSymbol s, ReflectRow xs) => ReflectRow (s # x :< xs) where
  repeatRow x = RConT inferName x (repeatRow x)
  {-# INLINE repeatRow #-}
