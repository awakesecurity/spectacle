{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}

-- |
--
-- @since 0.1.0.0
module Language.Spectacle.Interaction.View.WorldView
  ( -- * World Views
    WorldView (WorldView, _worldViewHash, _worldViewRec),
    worldViewHash,
    worldViewRec,

    -- ** Construction
    newWorldView,

    -- ** Pretty Printers
    ppWorldView,
    ppRecView,

    -- * Field Views
    FieldView (FieldView),
    type ViewableSignature,
    mapRecView,
  )
where

import Data.Kind
import Data.Text.Prettyprint.Doc
import Lens.Micro

import Data.Ascript
import Data.Context
import Data.Type.Rec
import Data.World
import Language.Spectacle.Checker.Fingerprint
import Language.Spectacle.Interaction.AnsiDoc

-- ---------------------------------------------------------------------------------------------------------------------

data WorldView sig = WorldView
  { _worldViewHash :: Fingerprint
  , _worldViewRec :: RecT FieldView sig
  }

worldViewHash :: Lens' (WorldView sig) Fingerprint
worldViewHash = lens _worldViewHash \WorldView {..} x -> WorldView {_worldViewHash = x, ..}
{-# INLINE worldViewHash #-}

worldViewRec :: Lens' (WorldView sig) (RecT FieldView sig)
worldViewRec = lens _worldViewRec \WorldView {..} x -> WorldView {_worldViewRec = x, ..}
{-# INLINE worldViewRec #-}

newWorldView :: ViewableSignature sig => World sig -> WorldView sig
newWorldView (World fingerprint xs) = WorldView fingerprint (mapRecView xs)

ppWorldView :: ViewableSignature sig => Int -> String -> WorldView sig -> AnsiDoc
ppWorldView depth action WorldView {..} =
  "<world:"
    <> pretty depth
    <> ":"
    <> prettyAnsi _worldViewHash
    <> "> from"
    <+> pretty action
    <> line
    <> indent 2 (ppRecView _worldViewRec)

ppRecView :: ViewableSignature sig => RecT FieldView sig -> AnsiDoc
ppRecView = vsep . foldFieldViews
  where
    foldFieldViews :: ViewableSignature sig => RecT FieldView sig -> [AnsiDoc]
    foldFieldViews = \case
      RNilT -> mempty
      RConT name (FieldView x) xs -> "#" <> fieldView name x : foldFieldViews xs

    fieldView :: AnsiPretty a => Name s -> a -> AnsiDoc
    fieldView name x = pretty (show name) <+> "=" <+> prettyAnsi x

-- ---------------------------------------------------------------------------------------------------------------------

data FieldView :: Type -> Type where
  FieldView :: AnsiPretty a => a -> FieldView a

type ViewableSignature :: Context -> Constraint
type family ViewableSignature sig where
  ViewableSignature 'CtxtNil = ()
  ViewableSignature ( 'CtxtCon (s # a) sig) = (AnsiPretty a, ViewableSignature sig)

mapRecView :: ViewableSignature sig => Rec sig -> RecT FieldView sig
mapRecView = \case
  RNil -> RNilT
  RCon name x xs -> RConT name (FieldView x) (mapRecView xs)
