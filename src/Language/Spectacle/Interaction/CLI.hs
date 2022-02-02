{-# LANGUAGE OverloadedStrings #-}

-- | This module exports the 'CLI' monad, an abstraction over command-line interactions such as emitting logs and
-- messages from the model checker per options declared by a user.
--
-- @since 0.1.0.0
module Language.Spectacle.Interaction.CLI
  ( -- * The CLI Monad
    CLI (CLI),
    unCLI,

    -- ** Running CLI
    runCLI,

    -- ** CLI Operations
    cliPutDoc,

    -- ** CLI Documents
    cliResultDoc,

    -- * CLI Context
    ContextCLI (ContextCLI),
    ctxOpts,
    ctxHandle,

    -- ** Construction
    newContextCLI,
  )
where

import Control.Monad (unless)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Reader (MonadReader, ReaderT, asks, runReaderT)
import Prettyprinter (Doc, annotate, line, unAnnotate, (<+>))
import Prettyprinter.Render.Terminal (AnsiStyle, Color (Green, Red), bold, color, hPutDoc)
import System.IO (Handle, hClose)

import Language.Spectacle.Interaction.Options (OptsCLI, isStdout, optsLogOutput, optsOnlyTrace)
import qualified Language.Spectacle.Interaction.Options as Opts

-- ---------------------------------------------------------------------------------------------------------------------

-- | The 'CLI' monad is a @'ReaderT' 'IO'@ carrying context of command-line options.
--
-- @since 0.1.0.0
newtype CLI a = CLI
  {unCLI :: ReaderT ContextCLI IO a}
  deriving stock (Functor)
  deriving
    (Applicative, Monad, MonadIO, MonadReader ContextCLI)
    via ReaderT ContextCLI IO

-- | Lower 'CLI' into 'IO' given command-line options.
--
-- @since 0.1.0.0
runCLI :: CLI a -> OptsCLI -> IO a
runCLI cli opts = do
  ctx <- newContextCLI opts
  ret <- runReaderT (unCLI cli) ctx

  unless (Opts.isStdout $ optsLogOutput opts) do
    -- Close the handle to the log-output buffer created by
    -- 'newContextCLI'/'handleFrom' after the command-line
    -- interaction has been completed, if the handle was not
    -- to System.IO.stdout
    hClose (ctxHandle ctx)

  pure ret

-- | @'cliPutDoc' doc@ emits the given @doc@ using CLI context's buffer handle.
--
-- @since 0.1.0.0
cliPutDoc :: Doc AnsiStyle -> CLI ()
cliPutDoc doc = do
  handle <- asks ctxHandle
  output <- asks (optsLogOutput . ctxOpts)
  if isStdout output
    then liftIO do
      hPutDoc handle doc
    else liftIO do
      -- Clear 'AnsiStyle' annotations from the 'Doc' for
      -- buffers other than stdout, since they can not be
      -- rendered by the terminal.
      hPutDoc handle (unAnnotate doc)

-- | @'cliResultDoc' succeeded@ for a boolean flag @succeeded@ indicating if the model checker encountered an error or
-- not, lays out the document containing:
--
-- * The type of run the model checker took (either "model check" or "trace").
-- * The result of the run (either "success" or "failure")
--
-- @since 0.1.0.0
cliResultDoc :: Bool -> CLI (Doc AnsiStyle)
cliResultDoc succeeded = do
  runType <- asks (mkRunTypeDoc . ctxOpts)
  pure ("specification:" <+> runType <> ":" <+> resultDoc <> line)
  where
    mkRunTypeDoc opts
      | optsOnlyTrace opts = "trace"
      | otherwise = "model check"

    resultDoc
      | succeeded = annotate (bold <> color Green) "success"
      | otherwise = annotate (bold <> color Red) "failure"

-- ---------------------------------------------------------------------------------------------------------------------

-- | 'ContextCLI' is the collection frequently needed information for command-line interaction.
--
-- Note: the preferred method of construction for 'ContextCLI' is via 'newContextCLI'.
--
-- @since 0.1.0.0
data ContextCLI = ContextCLI
  { ctxOpts :: OptsCLI
  , ctxHandle :: Handle
  }
  deriving stock (Show)

-- | Constructs a 'ContextCLI' from a set of command-line options 'OptsCLI'.
--
-- @since 0.1.0.0
newContextCLI :: OptsCLI -> IO ContextCLI
newContextCLI opts = ContextCLI opts <$> Opts.handleFrom (optsLogOutput opts)
{-# INLINE newContextCLI #-}
