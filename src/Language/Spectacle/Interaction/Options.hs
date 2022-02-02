-- | Command-line interface options.
--
-- @since 0.1.0.0
module Language.Spectacle.Interaction.Options
  ( -- * CLI Options
    OptsCLI (OptsCLI),
    optsLogGraph,
    optsOnlyTrace,
    optsLogOutput,

    -- * CLI Parser
    execOptsCLI,
    parseOptsCLI,

    -- ** "only-trace" option
    pOnlyTrace,

    -- ** "log" option
    pLogGraph,

    -- ** "output" Option
    OutputOpt (OptStdout, OptPath),
    isStdout,
    handleFrom,
    pOutputOpt,
    pOutputPath,
  )
where

import Control.Applicative (Alternative ((<|>)))
import Options.Applicative (Parser, execParser, help, idm, info, long, metavar, short, strOption, switch)
import System.IO (BufferMode (LineBuffering), Handle, IOMode (ReadWriteMode), hSetBuffering, openFile, stdout)

-- ---------------------------------------------------------------------------------------------------------------------

-- |
--
-- @since 0.1.0.0
data OptsCLI = OptsCLI
  { optsLogGraph :: Bool
  , optsOnlyTrace :: Bool
  , optsLogOutput :: OutputOpt
  }
  deriving (Eq, Show)

-- |
--
-- @since 0.1.0.0
execOptsCLI :: IO OptsCLI
execOptsCLI = execParser (info parseOptsCLI idm)

-- |
--
-- @since 0.1.0.0
parseOptsCLI :: Parser OptsCLI
parseOptsCLI =
  OptsCLI
    <$> pLogGraph
    <*> pOnlyTrace
    <*> pOutputOpt

-- ---------------------------------------------------------------------------------------------------------------------

-- | CLI parser that consumes the "only-trace" flag.
--
-- @since 0.1.0.0
pOnlyTrace :: Parser Bool
pOnlyTrace =
  switch
    ( long "only-trace"
        <> short 't'
        <> help "Disable property checking and only trace a specification"
    )

-- ---------------------------------------------------------------------------------------------------------------------

-- | CLI parser that consumes the "log" flag.
--
-- @since 0.1.0.0
pLogGraph :: Parser Bool
pLogGraph =
  switch
    ( long "log"
        <> short 'l'
        <> help "Graph the model checker trace"
    )

-- ---------------------------------------------------------------------------------------------------------------------

-- | CLI option datatype holding either a filepath to emit model checker logs to.
--
-- * @'OutputPath' str@ is a filepath to write logs to.
-- * 'OutputStdout' represents stdout as the chosen output location.
--
-- @since 0.1.0.0
data OutputOpt
  = OptStdout
  | OptPath FilePath
  deriving (Eq, Show)

-- | Is the output buffer stdout?
--
-- @since 0.1.0.0
isStdout :: OutputOpt -> Bool
isStdout opt = opt == OptStdout

-- | @'handleFrom' opt@ will extract the file hand from the given 'OutputOpt' @opt@.
--
-- @since 0.1.0.0
handleFrom :: OutputOpt -> IO Handle
handleFrom opt = do
  handle <- case opt of
    OptStdout -> pure stdout
    OptPath fp -> openFile fp ReadWriteMode

  hSetBuffering handle LineBuffering
  pure handle

-- | CLI parser that consumes the result of 'pOutputPath' if an output path is provided, otherwise 'OutputStd' is
-- returned by default and logs will be written to stdout.
--
-- @since 0.1.0.0
pOutputOpt :: Parser OutputOpt
pOutputOpt = pOutputPath <|> pure OptStdout

-- | CLI parser that consumes a filepath to emit logs to.
--
-- @since 0.1.0.0
pOutputPath :: Parser OutputOpt
pOutputPath = OptPath <$> parser
  where
    parser =
      strOption
        ( long "output"
            <> short 'o'
            <> metavar "OUTPUT"
            <> help "The log output path"
        )

-- ---------------------------------------------------------------------------------------------------------------------
