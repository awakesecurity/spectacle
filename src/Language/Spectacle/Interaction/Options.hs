{-# OPTIONS_HADDOCK show-extensions #-}

-- |
-- Module      :  Language.Spectacle.Interaction.Options
-- Copyright   :  (c) Arista Networks, 2022-2023
-- License     :  Apache License 2.0, see LICENSE
--
-- Stability   :  stable
-- Portability :  non-portable (GHC extensions)
--
-- Command-line interface options.
--
-- @since 1.0.0
module Language.Spectacle.Interaction.Options (
  -- * OptionsCLI
  OptionsCLI (..),

  -- * OutputOption
  OutputOption (..),

  -- ** I/O
  openOutput,

  -- * CLI Options
  getOptionsCLI,

  -- ** Parsers
  parseOptionsCLI,
  parseDiagramOption,
  parseNoCheckOption,
  parseRunTraceOption,
  parseOutputOption,
) where

import Data.Foldable (fold)
import Data.String (IsString (..))
import Options.Applicative (FlagFields, Mod, OptionFields, Parser)
import Options.Applicative qualified as Options
import System.IO (Handle)
import System.IO qualified as System

--------------------------------------------------------------------------------

defineStringOption :: IsString s => [Mod OptionFields s] -> Parser s
defineStringOption = Options.strOption . fold

defineSwitchOption' :: [Mod FlagFields Bool] -> Parser Bool
defineSwitchOption' = Options.switch . fold

-- OptionsCLI ------------------------------------------------------------------

-- | 'OptionsCLI' is a record storing all command-line options that can be used
-- to configure the model checker's behavior.
--
-- @since 1.0.0
data OptionsCLI = OptionsCLI
  { log_diagram :: Bool
  -- ^ Should the state diagram be drawn?
  --
  -- @since 1.0.0
  , run_check :: Bool
  -- ^ Should the model checker check temporal properties?
  --
  -- @since 1.0.1
  , run_trace :: Bool
  -- ^ Should the model checker trace the state space of a specification?
  --
  -- @since 1.0.1
  , output_path :: OutputOption
  -- ^ The output path for logs produced by CLI.
  --
  -- @since 1.0.0
  }
  deriving (Eq, Show)

-- OutputOption ----------------------------------------------------------------

-- | The 'OutputOption' datatype is the sum of all filepaths that can be
-- specified from Spectacles command-line options.
--
-- @since 1.0.0
data OutputOption
  = -- | The output path representing @('System.stderr' :: 'Handle')@.
    --
    -- @since 1.0.1
    OutputStderr
  | -- | The output path representing @('System.stdout' :: 'Handle')@.
    --
    -- @since 1.0.0
    OutputStdout
  | -- | Output path for an arbitrary 'FilePath'.
    --
    -- @since 1.0.0
    OutputPath FilePath
  deriving (Eq, Ord, Show)

-- | @since 1.0.1
instance IsString OutputOption where
  fromString str
    | str == "stderr" = OutputStderr
    | str == "stdout" = OutputStdout
    | otherwise = OutputPath str
  {-# INLINE CONLIKE fromString #-}

-- | @since 1.0.1
instance Read OutputOption where
  readsPrec _ str = [(fromString str, str)]
  {-# INLINE readsPrec #-}

-- OutputOption - I/O ----------------------------------------------------------

-- | Similar to 'System.openFile', but instead obtains a write-only file
-- 'Handle' from a 'OutputOption'.
--
-- @since 1.0.1
openOutput :: OutputOption -> IO Handle
openOutput OutputStderr = pure System.stderr
openOutput OutputStdout = pure System.stdout
openOutput (OutputPath filepath) = System.openFile filepath System.WriteMode

-- CLI Options -----------------------------------------------------------------

-- | Similar 'System.getArgs', but parses all program arguments with the
-- 'parseOptionsCLI' parser.
--
-- @since 1.0.1
getOptionsCLI :: IO OptionsCLI
getOptionsCLI =
  Options.customExecParser
    (Options.prefs Options.showHelpOnEmpty)
    (Options.info (Options.helper <*> parseOptionsCLI) Options.idm)

-- CLI Options - Parsers -------------------------------------------------------

-- | The 'parseOptionsCLI' function parses each of the CLI options, and collects
-- each of the parsed values into a 'OptionsCLI' record.
--
-- @since 1.0.1
parseOptionsCLI :: Parser OptionsCLI
parseOptionsCLI =
  OptionsCLI
    <$> parseDiagramOption
    <*> (not <$> parseNoCheckOption)
    <*> parseRunTraceOption
    <*> parseOutputOption

-- | Parse the "--no-check" CLI flag. Enabling the "--no-check" flag will
-- disable property checking in the model checker.
--
-- __Default:__ @('True' :: 'Bool')@
--
-- @since 1.0.1
parseNoCheckOption :: Parser Bool
parseNoCheckOption =
  defineSwitchOption'
    [ Options.long "no-check"
    , Options.help "Disable property checking."
    ]

-- | Parse the @"--trace"@ CLI flag. Enabling the @"--trace"@ flag enables
-- "trace mode" for model checker. If the @"--trace"@ flag is combined with the
-- @"--no-check"@ flag, the model checker  will only enumerate the states that
-- are reachable by a specification's actions without performing property checks
-- on the states reached.
--
-- __Default:__ @('False' :: 'Bool')@
--
-- @since 1.0.1
parseRunTraceOption :: Parser Bool
parseRunTraceOption =
  defineSwitchOption'
    [ Options.long "trace"
    , Options.short 't'
    , Options.help "Enable tracing the specification's state space."
    ]

-- | Parse the @"--diagram"@ CLI flag. If the @"--trace"@ flag is enabled, then
-- enabling the @"--diagram"@ flag will render the specification's state space
-- as a diagram and write it to output path specified by the "--output" option.
--
-- __Default:__ @('False' :: 'Bool')@
--
-- @since 1.0.1
parseDiagramOption :: Parser Bool
parseDiagramOption =
  defineSwitchOption'
    [ Options.long "diagram"
    , Options.help "Output a diagram of the model checker trace."
    ]

-- | Parse the @"--output"@ CLI option.
--
--   * If @"--output stderr"@ is passed, then all logs and output from the model
--     checker will be rendered to @('System.stderr' :: 'Handle')@.
--
--   * If @"--output stdout"@ is passed, then all logs and output from the model
--     checker will be rendered to @('System.stdout' :: 'Handle')@.
--
--   * Otherwise, any argument given to the @"--output"@ option is assumed to be
--     a filepath, and all output will be rendered to a file at that location.
--
-- __Default:__ @('System.stdout' :: 'Handle')@
--
-- @since 1.0.1
parseOutputOption :: Parser OutputOption
parseOutputOption =
  defineStringOption
    [ Options.long "output"
    , Options.short 'o'
    , Options.help "The log output path"
    , Options.value OutputStdout
    ]
