-- |
--
-- @since 0.1.0.0
module Language.Spectacle.Interaction.Parse
  ( -- * Parsers
    ReplayOpts (ReplayOpts),
    replayFrom,
    replayTo,
    replayDepth,
    parseReplayOpts,
    parseFingerprint,
    parseDepth,
  )
where

import Control.Monad
import Data.Bits
import Data.Char
import Data.Functor
import Text.Megaparsec
import Text.Megaparsec.Char

import Data.Foldable (foldr')
import Language.Spectacle.Checker.Fingerprint

-- ---------------------------------------------------------------------------------------------------------------------

type ParserCli = Parsec String String

data ReplayOpts = ReplayOpts
  { replayFrom :: Fingerprint
  , replayTo :: Fingerprint
  , replayDepth :: Int
  }
  deriving (Show)

parseReplayOpts :: ParserCli ReplayOpts
parseReplayOpts = between (string "+replay" <* space1) (space1 *> string "-replay") do
  ReplayOpts
    <$> (space *> string "-from=" *> parseFingerprint)
    <*> (space *> string "-to=" *> parseFingerprint)
    <*> (space *> string "-d=" *> parseDepth)

parseFingerprint :: ParserCli Fingerprint
parseFingerprint =
  string "0x" *> replicateM 8 hexDigitChar
    <&> foldr' (\(i, c) y -> shiftL (fromHexChar c) (4 * i) .|. y) 0 . zip [0 :: Int ..]
    <&> Fingerprint
  where
    fromHexChar :: Char -> Int
    fromHexChar c
      | isDigit c = fromEnum c - 48
      | isUpper c = fromEnum c - 55
      | otherwise = fromEnum c - 87

parseDepth :: ParserCli Int
parseDepth = read <$> takeWhile1P Nothing isDigit
