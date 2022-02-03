{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE UndecidableInstances #-}

-- | Model checker errors.
--
-- @since 1.0.0
module Language.Spectacle.Model.ModelError
  ( -- * Model Errors
    ModelError (InitialError, RuntimeError, RefutedError),

    -- ** Pretty Printing
    ppModelError,
    ppInitialError,
    ppRuntimeError,

    -- * Property Errors
    TemporalError (TemporalError),
    errorModality,
    errorPropName,
    errorPrimes,
    errorPlains,

    -- ** Pretty Printing
    ppTemporalError,
  )
where

import Data.Kind (Type)
import GHC.TypeLits (Symbol)
import Prettyprinter (Doc, align, indent, line, viaShow, vsep, (<+>))
import Prettyprinter.Render.Terminal (AnsiStyle)

import Data.Type.Rec (Ascribe, HasDict)
import Data.World (World, ppWorldListed)
import Language.Spectacle.Exception.RuntimeException (RuntimeException)
import Language.Spectacle.Specification.Prop (Modality, ppModality)

-- ---------------------------------------------------------------------------------------------------------------------

-- | 'TemporalError' captures information about a temporal property refutation.
--
-- @since 1.0.0
data ModelError :: [Ascribe Symbol Type] -> Type where
  InitialError ::
    ModelError ctx
  RuntimeError ::
    RuntimeException ->
    ModelError ctx
  RefutedError ::
    TemporalError ctx ->
    ModelError ctx

-- | @since 1.0.0
deriving instance HasDict Show ctx => Show (ModelError ctx)

ppModelError :: HasDict Show ctx => ModelError ctx -> Doc AnsiStyle
ppModelError = \case
  InitialError -> ppInitialError
  RuntimeError exc -> ppRuntimeError exc
  RefutedError err -> ppTemporalError err

ppInitialError :: Doc AnsiStyle
ppInitialError = "no initial states resulted from evaluating the initial actions"

ppRuntimeError :: RuntimeException -> Doc AnsiStyle
ppRuntimeError exc = "runtime error:" <+> viaShow exc

-- ---------------------------------------------------------------------------------------------------------------------

-- | 'TemporalError' captures information about a temporal property refutation.
--
-- @since 1.0.0
data TemporalError ctx = TemporalError
  { errorModality :: Modality
  , errorPropName :: String
  , errorPrimes :: Maybe (World ctx)
  , errorPlains :: Maybe (World ctx)
  }

-- | @since 1.0.0
deriving instance HasDict Show ctx => Show (TemporalError ctx)

ppTemporalError :: HasDict Show ctx => TemporalError ctx -> Doc AnsiStyle
ppTemporalError TemporalError {..} =
  "refuted property:"
    <+> ppModality errorModality
    <+> viaShow errorPropName
    <> line
    <> stateList
  where
    stateList =
      let plains = align . mappend "from: " . vsep . ppWorldListed <$> errorPlains
          primes = align . mappend "to: " . vsep . ppWorldListed <$> errorPrimes
          states = foldMap (maybe [] pure) [plains, primes]
       in vsep (map (indent 2 . mappend "* ") states)
