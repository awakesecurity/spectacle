{-# LANGUAGE ApplicativeDo #-}

-- |
--
-- @since 0.1.0.0
module Language.Spectacle.Interaction.Args where

import Control.Applicative (liftA2)
import Control.Monad (replicateM)
import Data.Bits (rotateL)
import Data.Foldable (foldr')
import Data.Functor (($>))
import Data.List.NonEmpty (NonEmpty ((:|)))
import qualified Data.Set as Set
import Text.Megaparsec
  ( ErrorItem (Tokens),
    MonadParsec (token, try),
    Parsec,
    between,
    customFailure,
    failure,
    many,
    parseMaybe,
    single,
    (<|>),
  )
import Text.Megaparsec.Char (digitChar, string)

import Language.Spectacle.Checker.Fingerprint (Fingerprint (Fingerprint))

-- ---------------------------------------------------------------------------------------------------------------------

type ParseCLI = Parsec [String] [String]

inclusion :: SubparseCLI a -> ParseCLI a
inclusion p = token (parseMaybe p) Set.empty

type SubparseCLI = Parsec [String] String

data CmdArgs = CmdArgs
  { argTrace :: Maybe ReplayOpts
  }
  deriving (Show)

parseCmdArgs :: ParseCLI CmdArgs
parseCmdArgs =
  CmdArgs
    <$> (try (Just <$> tokenReplayOpts) <|> pure Nothing)

data ReplayOpts
  = ReplaySubtreeOpts
      {-# UNPACK #-} !Fingerprint
      {-# UNPACK #-} !Fingerprint
  | ReplayDepthOpts
      {-# UNPACK #-} !Int
      {-# UNPACK #-} !Int
  | ReplayBoundOpts
      {-# UNPACK #-} !Int
  deriving (Show)

tokenReplayOpts :: ParseCLI ReplayOpts
tokenReplayOpts = between (single "+replay") (single "-replay") do
  try (uncurry ReplaySubtreeOpts <$> parseSubtreeOpts)
    <|> try (uncurry ReplayDepthOpts <$> parseDepthOpts)
    <|> try (ReplayBoundOpts <$> parseBoundOpts)

-- | Parse an interval of fingerprints as a replayer option.
--
-- @
-- 'parseSubtreeOpts' "-from 0x01234567 -to 0x01234567"
-- @
--
-- @since 0.1.0.0
parseSubtreeOpts :: ParseCLI (Fingerprint, Fingerprint)
parseSubtreeOpts = liftA2 (,) (tokenVarArg "from" tokenFingerprint) (tokenVarArg "to" tokenFingerprint)

-- | Parse an ordered pair of minimum/maximum depths as a replayer option.
--
-- @
-- 'parseDepthOpts' "-from 10 -to 12"
-- @
--
-- @since 0.1.0.0
parseDepthOpts :: ParseCLI (Int, Int)
parseDepthOpts = liftA2 (,) (tokenVarArg "from" tokenDepth) (tokenVarArg "to" tokenDepth)

-- | Parse a single depth bound as a replayer option.
--
-- @
-- 'parseDepthOpts' "-bound 5"
-- @
--
-- @since 0.1.0.0
parseBoundOpts :: ParseCLI Int
parseBoundOpts = tokenVarArg "bound" tokenDepth

tokenDepth :: ParseCLI Int
tokenDepth = read <$> inclusion (many digitChar)

-- | Parse a CLI variable, which for @'tokenVarArg' "from" p@ is a string of the form "-from p(...)".
--
-- @since 0.1.0.0
tokenVarArg :: String -> ParseCLI a -> ParseCLI a
tokenVarArg varName rhs = prefixVar *> rhs
  where
    prefixVar :: ParseCLI ()
    prefixVar = single ("-" ++ varName) $> ()

tokenFingerprint :: ParseCLI Fingerprint
tokenFingerprint = do
  idxWords <- inclusion (zip [0 :: Int ..] <$> tokenHexWord32)
  let n :: Int = foldr' ((+) . conDigit) 0 idxWords

  pure (Fingerprint (fromIntegral n))
  where
    conDigit :: (Int, Int) -> Int
    conDigit (idx, d) = rotateL d (4 * idx)

tokenHexWord32 :: SubparseCLI [Int]
tokenHexWord32 = do
  digits <- string "0x" *> replicateM 8 tokenHex
  case digits of
    [] -> customFailure ["unexpected chars found attempting to parse hex-encoded Word32"]
    d : ds ->
      if length digits == 8
        then pure (d : ds)
        else failure (Just (Tokens (toEnum d :| map toEnum ds))) Set.empty

tokenHex :: SubparseCLI Int
tokenHex = token fromHexChar Set.empty
  where
    fromHexChar :: Char -> Maybe Int
    fromHexChar c
      | '0' <= c && c <= '9' = Just (fromEnum c - fromEnum '0')
      | 'A' <= c && c <= 'F' = Just (fromEnum c - fromEnum 'A' + 10)
      | 'a' <= c && c <= 'f' = Just (fromEnum c - fromEnum 'a' + 10)
      | otherwise = Nothing
