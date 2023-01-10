{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_HADDOCK show-extensions #-}

-- |
-- Module      :  Language.Spectacle.Interaction
-- Copyright   :  (c) Arista Networks, 2022-2023
-- License     :  Apache License 2.0, see LICENSE
--
-- Stability   :  stable
-- Portability :  non-portable (GHC extensions)
--
-- CLI interaction.
--
-- @since 1.0.0
module Language.Spectacle.Interaction (
  -- * CLI
  interaction,
  handleCheckInteraction,
  handleTraceInteraction,
) where

import Control.Monad (unless, when)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader (asks)
import Data.Either (isRight)
import Data.Functor.Tree (Tree)
import Data.Hashable (Hashable)
import Data.Type.Rec (HasDict)
import Data.World (World)
import Language.Spectacle.Interaction.CLI (CLI)
import Language.Spectacle.Interaction.CLI qualified as CLI
import Language.Spectacle.Interaction.Diagram (diagramFull, runDiagram)
import Language.Spectacle.Interaction.Options qualified as Options
import Language.Spectacle.Interaction.Options qualified as Opts
import Language.Spectacle.Interaction.Paths (toPointSet)
import Language.Spectacle.Model (modelcheck, modeltrace)
import Language.Spectacle.Model.ModelError (ModelError, ppModelError)
import Language.Spectacle.Specification (Specification)
import Prettyprinter (Doc)
import Prettyprinter qualified as Doc
import Prettyprinter.Render.Terminal (AnsiStyle, Color (..))
import Prettyprinter.Render.Terminal qualified as Doc

--------------------------------------------------------------------------------

interaction ::
  forall ctx acts form.
  (HasDict Eq ctx, HasDict Hashable ctx, HasDict Show ctx) =>
  Specification ctx acts form ->
  IO ()
interaction spec = do
  options <- Opts.getOptionsCLI

  CLI.runCLI options do
    if Options.run_check options
      then do
        checkResult <- liftIO (modelcheck spec)
        handleCheckInteraction checkResult

        when (Options.run_trace options) do
          traceResult <- liftIO (modeltrace spec)
          handleTraceInteraction traceResult

        modesDoc <- CLI.docRunModes (isRight checkResult)
        CLI.cliPutDoc (modesDoc <> Doc.line)
      else
        if Options.run_trace options
          then do
            result <- liftIO (modeltrace spec)
            modesDoc <- CLI.docRunModes (isRight result)

            handleTraceInteraction result

            unless (Options.log_diagram options) do
              CLI.cliPutDoc (docWarn ("running " <> style "--trace" <> " without enabling " <> style "--diagram"))

            CLI.cliPutDoc (modesDoc <> Doc.line)
          else do
            CLI.cliPutDoc (docWarn "nothing to do!")
  where
    docWarn :: Doc AnsiStyle -> Doc AnsiStyle
    docWarn doc = style "warning: " <> doc <> Doc.line

    style :: Doc AnsiStyle -> Doc AnsiStyle
    style = Doc.annotate (Doc.color Blue)

handleCheckInteraction ::
  HasDict Show ctx =>
  Either (ModelError ctx) [Tree (World ctx)] ->
  CLI ()
handleCheckInteraction result = do
  case result of
    Left err -> CLI.cliPutDoc (ppModelError err <> Doc.line)
    Right {} -> pure ()

handleTraceInteraction ::
  HasDict Show ctx =>
  Either (ModelError ctx) [Tree (World ctx)] ->
  CLI ()
handleTraceInteraction result = do
  case result of
    Left err -> CLI.cliPutDoc (ppModelError err <> Doc.line)
    Right trees -> do
      options <- asks CLI.ctx_options

      when (Options.log_diagram options) do
        let pointSet = foldMap toPointSet trees
        diagramDoc <- liftIO (runDiagram $ diagramFull pointSet)
        CLI.cliPutDoc (diagramDoc <> Doc.line)
