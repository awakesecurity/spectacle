{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE UndecidableInstances #-}

-- | Extensible records.
--
-- @since 1.0.0
module Data.Type.Rec
  ( -- * Extensible Records Transformer
    RecF (NilF, ConF),
    getF,
    setF,

    -- ** Construction
    concatF,

    -- ** Maps
    mapF,
    sequenceF,

    -- ** Destruction
    foldMapF,

    -- ** Pretty Printing
    ppRecListed,

    -- * Extensible Records
    Rec,
    pattern Nil,
    pattern Con,
    get,
    set,

    -- * Record Dictionaries
    Evident (Evident, Trivial),
    pattern NilE,
    pattern ConE,

    -- * HasDict
    HasDict,
    evident,

    -- * Has
    Has,

    -- * Re-export
    module Data.Ascript,
    module Data.Name,
  )
where

import Control.Applicative (liftA2)
import Data.Functor.Identity (Identity (Identity, runIdentity))
import Data.Hashable (Hashable (hashWithSalt), hashWithSalt)
import Data.Kind (Constraint, Type)
import Data.List (intercalate)
import GHC.TypeLits (KnownSymbol, Symbol)
import Prettyprinter (Doc, pretty, viaShow, (<+>))
import Prettyprinter.Render.Terminal (AnsiStyle)

import Data.Ascript (Ascribe, type (#))
import Data.Name (Name, inferName)
import Data.Type.List (type (++))

-- ---------------------------------------------------------------------------------------------------------------------

-- | Extensible record transformer.
--
-- @since 0.1.0.0
data RecF :: (k -> Type) -> [Ascribe Symbol k] -> Type where
  NilF :: RecF f '[]
  ConF :: Name s -> f a -> RecF f ctx -> RecF f (s # a ': ctx)

-- | @'sequenceA'@ for an extensible record.
sequenceF :: Applicative f => RecF f ctx -> f (Rec ctx)
sequenceF NilF = pure Nil
sequenceF (ConF name field xs) = liftA2 (Con name) field (sequenceF xs)

-- | @'map'@ over a set of fields in a given extensible record.
mapF :: (forall s a. Name s -> f a -> g a) -> RecF f ctx -> RecF g ctx
mapF _ NilF = NilF
mapF f (ConF name field xs) = ConF name (f name field) (mapF f xs)

-- | Fold over an extensible record to produce a monoidal result.
foldMapF :: Monoid m => (forall s a. Name s -> f a -> m) -> RecF f ctx -> m
foldMapF _ NilF = mempty
foldMapF k (ConF name field xs) = k name field <> foldMapF k xs

-- | Concatenate two compatible records (and their fields) into one.
concatF :: RecF f ctx -> RecF f ctx' -> RecF f (ctx ++ ctx')
concatF NilF ys = ys
concatF (ConF name x xs) ys = ConF name x (concatF xs ys)

-- | Pretty-print an extensible record as a list of fields.
ppRecListed :: HasDict Show ctx => Rec ctx -> [Doc AnsiStyle]
ppRecListed rs =
  case evident @Show rs of
    ConE n x xs -> pretty n <+> "=" <+> viaShow x : ppRecListed xs
    NilE -> []

-- ---------------------------------------------------------------------------------------------------------------------

-- | 'Rec' is an extensible record.
--
-- @since 0.1.0.0
type Rec ctx = RecF Identity ctx

-- | A synonym of 'NilF' specialize to 'Rec'.
--
-- @since 0.1.0.0
pattern Nil :: () => '[] ~ ctx => Rec ctx
pattern Nil = NilF

-- | A synonym of 'ConF' specialize to 'Rec'.
--
-- @since 0.1.0.0
pattern Con :: () => (s # a ': xs) ~ ctx => Name s -> a -> Rec xs -> Rec ctx
pattern Con name field xs = ConF name (Identity field) xs

{-# COMPLETE Nil, Con #-}

-- | @since 0.1.0.0
instance HasDict Eq ctx => Eq (Rec ctx) where
  -- Nominal equality
  Nil == Nil = True
  (evident @Eq -> ConE _ x xs) == (evident @Eq -> ConE _ y ys)
    | x == y = xs == ys
    | otherwise = False

-- | @since 0.1.0.0
instance HasDict Show ctx => Show (Rec ctx) where
  show Nil = "Rec {}"
  show rs@Con {} = "Rec {" ++ intercalate "; " (go $ evident rs) ++ "}"
    where
      go :: forall x. Evident Show x -> [String]
      go NilE = []
      go (ConE name field xs) = (show name ++ " = " ++ show field) : go (evident @Show xs)


-- | @since 0.1.0.0
instance (HasDict Eq ctx, HasDict Hashable ctx) => Hashable (Rec ctx) where
  hashWithSalt salt rs = case evident @Eq rs of
    NilE -> salt
    ConE {} -> case evident @Hashable rs of
      ConE _ x xs -> hashWithSalt (hashWithSalt salt x) xs

-- | Set a field @f@ to a value @a@ in an extensible record.
set :: Has s a ctx => Name s -> a -> Rec ctx -> Rec ctx
set name x = setF name (Identity x)

-- | Get the value of a record field.
get :: Has s a ctx => Name s -> Rec ctx -> a
get n r = runIdentity (getF n r)

-- ---------------------------------------------------------------------------------------------------------------------

-- | @'Evident' c ctx@ captures dictionary evidence of @Rec ctx@ for the typeclass @c@.
--
-- @since 0.1.0.0
data Evident :: (Type -> Constraint) -> [Ascribe Symbol Type] -> Type where
  Trivial :: Evident c '[]
  Evident :: (c a, HasDict c ctx) => Rec (s # a ': ctx) -> Evident c (s # a ': ctx)

-- | A synonym of 'Nil' specialize to 'Evident'.
--
-- @since 0.1.0.0
pattern NilE :: () => '[] ~ ctx => Evident c ctx
pattern NilE = Trivial

-- | A synonym of 'Con' specialize to 'Evident'.
--
-- @since 0.1.0.0
pattern ConE :: () => (c a, HasDict c xs, (s # a ': xs) ~ ctx) => Name s -> a -> Rec xs -> Evident c ctx
pattern ConE name field xs = Evident (Con name field xs)

{-# COMPLETE NilE, ConE #-}

-- ---------------------------------------------------------------------------------------------------------------------

-- | @'HasDict' c ctx@ is what it means for a @Rec ctx@ to be an instance of @c@.
--
-- * @Rec CNil@ trivially fulfills any constraint.
--
-- * @Rec (x :< ctx)@ fulfills @c@ iff. @c a@ and @HasDict c (Rec ctx)@ are fulfilled.
--
-- @since 0.1.0.0
class HasDict c ctx where
  -- | Provide evidence constraints for the given record.
  evident :: Rec ctx -> Evident c ctx

-- | @since 0.1.0.0
instance HasDict c '[] where
  evident = const Trivial
  {-# INLINE CONLIKE evident #-}

-- | @since 0.1.0.0
instance (c a, HasDict c ctx) => HasDict c (s # a ': ctx) where
  evident = Evident
  {-# INLINE CONLIKE evident #-}

-- ---------------------------------------------------------------------------------------------------------------------

-- | @'Has' s a ctx@ is the constraint that a @'Rec' ctx@ have a field @s@ of type @a@.
--
-- @since 0.1.0.0
class Has s a ctx | ctx s -> a where
  -- | Get a value from a specific field in a record
  getF :: Name s -> RecF f ctx -> f a

  -- | Set the value for a record field.
  setF :: Name s -> f a -> RecF f ctx -> RecF f ctx

-- | @since 0.1.0.0
instance {-# OVERLAPS #-} Has s a (s # a ': ctx) where
  getF _ (ConF _ x _) = x

  setF _ x (ConF name _ r) = ConF name x r

-- | @since 0.1.0.0
instance Has s a ctx => Has s a (s' # a' ': ctx) where
  getF name (ConF _ _ r) = getF name r

  setF name x (ConF name' y r) = ConF name' y (setF name x r)

-- |
--
-- @since 0.1.0.0
class ReflectRow ctx where
  repeatRow :: (forall a. f a) -> RecF f ctx

-- | @since 0.1.0.0
instance ReflectRow '[] where
  repeatRow _ = NilF

-- | @since 0.1.0.0
instance (KnownSymbol s, ReflectRow xs) => ReflectRow (s # x ': xs) where
  repeatRow x = ConF inferName x (repeatRow x)
