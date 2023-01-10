{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_HADDOCK show-extensions #-}

-- |
-- Module      :  Language.Spectacle.Interaction.CLI
-- Copyright   :  (c) Arista Networks, 2022-2023
-- License     :  Apache License 2.0, see LICENSE
--
-- Stability   :  stable
-- Portability :  non-portable (GHC extensions)
--
-- This module exports the 'CLI' monad, an abstraction over command-line
-- interactions such as emitting logs and messages from the model checker per
-- options declared by a user.
--
-- @since 1.0.0
module Language.Spectacle.Interaction.CLI (
  -- * The CLI Monad
  CLI (..),

  -- ** Running CLI
  runCLI,

  -- ** CLI Operations
  cliPutDoc,

  -- ** CLI Documents
  docRunModes,
  docCheckMode,

  -- * CLI Context
  ContextCLI (..),

  -- ** Construction
  newContextCLI,
) where

import Control.Monad.IO.Class (MonadIO (..))
import Control.Monad.Reader (MonadReader, ReaderT, asks, runReaderT)
import Data.List qualified as List
import Language.Spectacle.Interaction.Options (OptionsCLI)
import Language.Spectacle.Interaction.Options qualified as Options
import Prettyprinter (Doc)
import Prettyprinter qualified as Doc
import Prettyprinter.Render.Terminal (AnsiStyle, Color (..))
import Prettyprinter.Render.Terminal qualified as Doc
import System.IO (Handle)
import System.IO qualified as System
import Data.Foldable (fold)

--------------------------------------------------------------------------------

-- | The 'CLI' monad is a @'ReaderT' 'IO'@ carrying context of command-line options.
--
-- @since 1.0.0
newtype CLI a = CLI
  {unCLI :: ReaderT ContextCLI IO a}
  deriving stock (Functor)
  deriving
    (Applicative, Monad, MonadIO, MonadReader ContextCLI)
    via ReaderT ContextCLI IO

-- | Lower 'CLI' into 'IO' given command-line options.
--
-- @since 1.0.0
runCLI :: OptionsCLI -> CLI a -> IO a
runCLI options cli = do
  context <- newContextCLI options
  result <- runReaderT (unCLI cli) context

  case Options.output_path (ctx_options context) of
    Options.OutputPath filepath -> do
      let handle = ctx_handle context

      -- Release the handle on the output buffer created by 'newContextCLI' and
      -- 'openOutput' after the command-line interaction session has been
      -- terminatred, if the handle was not 'System.IO.stdout' or
      -- 'System.IO.stderr'.
      putStrLn ("output written to: " <> filepath)
      System.hClose handle 

      pure result
    _ ->
      pure result

-- | @'cliPutDoc' doc@ emits the given @doc@ using CLI context's buffer handle.
--
-- @since 1.0.0
cliPutDoc :: Doc AnsiStyle -> CLI ()
cliPutDoc doc = do
  handle <- asks ctx_handle
  options <- asks ctx_options
  case Options.output_path options of
    Options.OutputPath {} -> liftIO do
      -- Clear 'AnsiStyle' annotations from the 'Doc' for
      -- buffers other than stdout, since they can not be
      -- rendered by the terminal.
      Doc.hPutDoc handle (Doc.unAnnotate doc)
    _ -> liftIO do
      Doc.hPutDoc handle doc

-- | Renders a 'Doc' describing each of the modes that are enabled for a model
-- checker run.
--
-- @since 1.0.1
docRunModes :: 
  -- | A 'Bool' value indicating if property checks were satisfied or refuted.
  Bool -> 
  -- | The rendered 'Doc'.
  CLI (Doc AnsiStyle)
docRunModes suceeded = do
  options <- asks ctx_options

  let noteCheckMode :: [Doc AnsiStyle]
      noteCheckMode
        | Options.run_check options = [docCheckMode suceeded]
        | otherwise = mempty

  let noteTraceMode :: [Doc AnsiStyle]
      noteTraceMode
        | Options.run_trace options = ["trace"]
        | otherwise = mempty

  pure (catModes (fold [noteCheckMode, noteTraceMode]))
  where
    catModes :: [Doc AnsiStyle] -> Doc AnsiStyle
    catModes = Doc.cat . List.intersperse (Doc.annotate (Doc.color Blue) "+")

-- | Renders the string "check" along with a parenthesized status indicator 
-- telling:
-- 
--   * If the specification's passed property checks, for 
--     @('docCheckMode' 'True' :: 'Doc' 'AnsiStyle')@.
--
--   * Or if the model checker refuted the specification's property checks, for
--     @('docCheckMode' 'False' :: 'Doc' 'AnsiStyle')@.
--
-- @since 1.0.1
docCheckMode :: 
  -- | A 'Bool' value indicating if property checks were satisfied or refuted.
  Bool -> 
  -- | The rendered 'Doc'.
  Doc AnsiStyle
docCheckMode succeeded
  | succeeded = "check" <> Doc.parens (style Green "pass")
  | otherwise = "check" <> Doc.parens (style Red "fail")
  where
    style :: Color -> Doc AnsiStyle -> Doc AnsiStyle
    style c = Doc.annotate (Doc.bold <> Doc.color c)

--------------------------------------------------------------------------------

-- | 'ContextCLI' is the collection frequently needed information for 
-- command-line interaction.
--
-- __Note:__ the preferred method of construction for 'ContextCLI' is via 
-- 'newContextCLI'.
--
-- @since 1.0.0
data ContextCLI = ContextCLI
  { -- | The options configured for the interactive session. 
    -- 
    -- @since 1.0.1
    ctx_options :: OptionsCLI
  , -- | The file 'Handle' to the interactive session will output logs to.
    -- 
    -- @since 1.0.1
    ctx_handle :: Handle
  }
  deriving stock (Show)

-- | Constructs a 'ContextCLI' from a set of command-line options 'OptionsCLI'.
--
-- @since 1.0.0
newContextCLI :: OptionsCLI -> IO ContextCLI
newContextCLI options = do
  let path = Options.output_path options
  handle <- Options.openOutput path
  pure (ContextCLI options handle)
