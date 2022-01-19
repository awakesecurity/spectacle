-- |
--
-- @since 0.1.0.0
module Language.Spectacle.Interaction.CLI where

import System.Environment
import System.Exit

import Language.Spectacle.Interaction.Args
import Text.Megaparsec

-- ---------------------------------------------------------------------------------------------------------------------

handleErrsCLI :: Either (ParseErrorBundle [String] [String]) CmdArgs -> IO CmdArgs
handleErrsCLI = \case
  Right cmdArgs -> pure cmdArgs
  Left (ParseErrorBundle errs st) -> do
    print errs
    print st
    exitFailure

getArgsCLI :: IO (Either (ParseErrorBundle [String] [String]) CmdArgs)
getArgsCLI =
  getArgs >>= \case
    [] -> undefined
    argStr : _ -> do
      let args = words argStr
          opts = parse parseCmdArgs "CLI" args
      pure opts
