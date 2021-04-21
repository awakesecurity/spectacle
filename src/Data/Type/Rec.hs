{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE UndecidableInstances #-}

module Data.Type.Rec
  ( type Rec,
    constMap,
    setRec,
    getRec,
    pattern RCon,
    RecT (RConT, RNil),
    fieldMap,
    constMapT,
    fields,
    type (.|),
    HasSel (..),

    -- * Re-exports
    Ascribe,
    type (#),
    Name (Name),
  )
where

import Control.Natural (type (~>))
import Data.Functor.Identity (Identity (Identity, runIdentity))
import Data.Kind (Constraint, Type)
import GHC.TypeLits (Symbol)

import Data.Ascript (Ascribe, AscriptName, AscriptType, type (#))
import Data.Name (Name (Name))

-- -------------------------------------------------------------------------------------------------

type Rec :: [Ascribe Symbol Type] -> Type
type Rec ctx = RecT Identity ctx

constMap :: Monoid (t a) => (forall s x. Name s -> x -> t a) -> Rec ctx -> t a
constMap f = constMapT (\name -> f name . Identity)
{-# INLINE constMap #-}

setRec :: forall ctx s a. s # a .| ctx => Name s -> a -> Rec ctx -> Rec ctx
setRec name x = setRecT name (Identity x)
{-# INLINE setRec #-}

getRec :: forall ctx s a. s # a .| ctx => Name s -> Rec ctx -> a
getRec n r = runIdentity (getRecT n r)
{-# INLINE getRec #-}

pattern RCon :: () => (s # a ': ctx) ~ ctx' => Name s -> a -> Rec ctx -> Rec ctx'
pattern RCon n x r <-
  RConT n (Identity x) r
  where
    RCon n x r = RConT n (Identity x) r
{-# COMPLETE RCon #-}

data RecT f ctx where
  RNil :: RecT f '[]
  RConT :: Name s -> f a -> RecT f ctx -> RecT f (s # a ': ctx)

instance Show (RecT f '[]) where
  show RNil = "RNil"

instance (Show (f a), Show (RecT f ctx)) => Show (RecT f (s # a ': ctx)) where
  show (RConT name x r) = show name ++ "=" ++ show x ++ " " ++ show r

fieldMap :: (f ~> g) -> RecT f ctx -> RecT g ctx
fieldMap _ RNil = RNil
fieldMap f (RConT name x r) = RConT name (f x) (fieldMap f r)
{-# INLINE fieldMap #-}

constMapT :: Monoid (t a) => (forall s x. Name s -> f x -> t a) -> RecT f ctx -> t a
constMapT _ RNil = mempty
constMapT f (RConT name x r) = f name x <> constMapT f r
{-# INLINE constMapT #-}

fields :: RecT f ctx -> [String]
fields = constMapT \name -> const [show name]
{-# INLINE fields #-}

infixr 3 .|
type (.|) :: Ascribe Symbol Type -> [Ascribe Symbol Type] -> Constraint
type a .| ctx = HasSel ctx (AscriptName a) (AscriptType a)

class HasSel ctx s a | ctx s -> a where
  getRecT :: Name s -> RecT f ctx -> f a

  setRecT :: Name s -> f a -> RecT f ctx -> RecT f ctx

-- | @since 0.1.0.0
instance {-# OVERLAPS #-} HasSel (s # a ': ctx) s a where
  getRecT _ (RConT _ x _) = x
  {-# INLINE getRecT #-}

  setRecT _ x (RConT name _ r) = RConT name x r
  {-# INLINE setRecT #-}

-- | @since 0.1.0.0
instance HasSel ctx s a => HasSel (t # b ': ctx) s a where
  getRecT name (RConT _ _ r) = getRecT name r
  {-# INLINE getRecT #-}

  setRecT name x (RConT name' y r) = RConT name' y (setRecT name x r)
  {-# INLINE setRecT #-}
