{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE UndecidableInstances #-}

-- |
--
-- @since 0.1.0.0
module Language.Spectacle.Interaction.AnsiDoc
  ( AnsiDoc,
    AnsiPretty (prettyAnsi),
  )
where

import Data.Kind
import Data.Proxy
import Data.Text.Prettyprint.Doc
import Data.Text.Prettyprint.Doc.Render.Terminal
import GHC.Generics
import GHC.TypeLits

import Data.Type.Rec
import Data.World
import Language.Spectacle.Checker.Fingerprint

-- ---------------------------------------------------------------------------------------------------------------------

type AnsiDoc = Doc AnsiStyle

class AnsiPretty a where
  prettyAnsi :: a -> AnsiDoc
  default prettyAnsi :: (Generic a, GAnsiPretty (Rep a)) => a -> AnsiDoc
  prettyAnsi = gPrettyAnsi . from
  {-# INLINE prettyAnsi #-}

-- | @since 0.1.0.0
instance AnsiPretty Fingerprint where
  prettyAnsi = pretty . show
  {-# INLINE prettyAnsi #-}

-- | @since 0.1.0.0
instance AnsiPretty (Rec sig) => AnsiPretty (World sig) where
  prettyAnsi (World fingerprint xs) = "<world:" <> prettyAnsi fingerprint <> ">" <> line <> indent 2 (prettyAnsi xs)
  {-# INLINE prettyAnsi #-}

-- | @since 0.1.0.0
instance {-# OVERLAPPABLE #-} Show a => AnsiPretty a where
  prettyAnsi = pretty . show
  {-# INLINE prettyAnsi #-}

-- ---------------------------------------------------------------------------------------------------------------------

type GAnsiPretty :: (Type -> Type) -> Constraint
class GAnsiPretty f where
  gPrettyAnsi :: f p -> AnsiDoc

-- | @since 0.1.0.0
instance GAnsiPretty V1 where
  gPrettyAnsi _ = "_|_"
  {-# INLINE gPrettyAnsi #-}

-- | @since 0.1.0.0
instance GAnsiPretty U1 where
  gPrettyAnsi U1 = "()"
  {-# INLINE gPrettyAnsi #-}

-- | @since 0.1.0.0
instance AnsiPretty c => GAnsiPretty (K1 i c) where
  gPrettyAnsi (K1 x) = prettyAnsi x
  {-# INLINE gPrettyAnsi #-}

-- | @since 0.1.0.0
instance (KnownSymbol m, GAnsiPretty f) => GAnsiPretty (D1 ( 'MetaData m n p nt) f) where
  gPrettyAnsi (M1 x) = pretty (symbolVal (Proxy @m)) <+> "::" <+> gPrettyAnsi x
  {-# INLINE gPrettyAnsi #-}

-- | @since 0.1.0.0
instance (KnownSymbol n, GAnsiPretty f) => GAnsiPretty (C1 ( 'MetaCons n p s) f) where
  gPrettyAnsi (M1 x) = pretty (symbolVal (Proxy @n)) <> line <> indent 2 (gPrettyAnsi x)
  {-# INLINE gPrettyAnsi #-}

-- | @since 0.1.0.0
instance GAnsiPretty f => GAnsiPretty (S1 ( 'MetaSel 'Nothing su ss ds) f) where
  gPrettyAnsi (M1 x) = gPrettyAnsi x
  {-# INLINE gPrettyAnsi #-}

-- | @since 0.1.0.0
instance (KnownSymbol mn, GAnsiPretty f) => GAnsiPretty (S1 ( 'MetaSel ( 'Just mn) su ss ds) f) where
  gPrettyAnsi (M1 x) = pretty (symbolVal (Proxy @mn)) <+> "=" <+> gPrettyAnsi x
  {-# INLINE gPrettyAnsi #-}

-- | @since 0.1.0.0
instance (GAnsiPretty f, GAnsiPretty g) => GAnsiPretty (f :+: g) where
  gPrettyAnsi (L1 x) = "L1(" <> gPrettyAnsi x <> ")"
  gPrettyAnsi (R1 x) = "R1(" <> gPrettyAnsi x <> ")"
  {-# INLINE gPrettyAnsi #-}

-- | @since 0.1.0.0
instance (GAnsiPretty f, GAnsiPretty g) => GAnsiPretty (f :*: g) where
  gPrettyAnsi (x :*: y) = gPrettyAnsi x <> line <> gPrettyAnsi y
  {-# INLINE gPrettyAnsi #-}

-- | @since 0.1.0.0
-- instance {\-# OVERLAPS #-\} (AnsiPretty c1, AnsiPretty c2) => GAnsiPretty (K1 i1 c1 :*: K1 i2 c2) where
--   gPrettyAnsi (x :*: y) = gPrettyAnsi x <+> ":*:" <+> gPrettyAnsi y
--   {\-# INLINE gPrettyAnsi #-\}
