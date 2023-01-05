{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}

-- | CLI interaction.
--
-- @since 1.0.0
module Language.Spectacle.Interaction
  ( -- * CLI
    interaction,
    handleInteraction,
  )
where

import Control.Monad (when)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader (asks)
import Data.Either (isRight)
import Data.Hashable (Hashable)

import Data.Functor.Tree (Tree)
import Data.Type.Rec (HasDict)
import Data.World (World)
import Language.Spectacle.Interaction.CLI (CLI, ContextCLI (ctxOpts), cliPutDoc, cliResultDoc, runCLI)
import Language.Spectacle.Interaction.Diagram (diagramFull, runDiagram)
import Language.Spectacle.Interaction.Options (OptsCLI (optsLogGraph, optsOnlyTrace))
import qualified Language.Spectacle.Interaction.Options as Opts
import Language.Spectacle.Interaction.Paths (toPointSet)
import Language.Spectacle.Model (modelcheck, modeltrace)
import Language.Spectacle.Model.ModelError (ModelError, ppModelError)
import Language.Spectacle.Specification (Specification)

import Prettyprinter (line)

-- ---------------------------------------------------------------------------------------------------------------------

interaction :: (HasDict Eq ctx, HasDict Hashable ctx, HasDict Show ctx) => Specification ctx acts form -> IO ()
interaction spec = do
  opts <- Opts.execOptsCLI
  result <-
    if optsOnlyTrace opts
      then modeltrace spec
      else modelcheck spec

  runCLI (handleInteraction result) opts

handleInteraction :: HasDict Show ctx => Either (ModelError ctx) [Tree (World ctx)] -> CLI ()
handleInteraction result =
  let status = isRight result
   in case result of
        Left err -> do
          cliPutDoc =<< cliResultDoc status
          cliPutDoc (ppModelError err)
          cliPutDoc line
        Right trees -> do
          isLogging <- asks (optsLogGraph . ctxOpts)
          when isLogging do
            let pointSet = foldMap toPointSet trees
            diagramDoc <- liftIO (runDiagram $ diagramFull pointSet)
            cliPutDoc (diagramDoc <> line)
          cliPutDoc =<< cliResultDoc status
