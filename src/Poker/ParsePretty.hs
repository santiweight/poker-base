{-# LANGUAGE OverloadedStrings #-}

module Poker.ParsePretty where

import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Void (Void)
import Poker.Utils (terror)
import Text.Megaparsec (MonadParsec, Parsec, empty, parseMaybe, (<?>))

type PrettyParser a = Parsec Void Text a

class ParsePretty a where
  parsePrettyP :: Ord e => MonadParsec e Text m => m a

tfailure :: MonadParsec e Text m => Text -> m a
tfailure err = empty <?> T.unpack err

parsePretty :: ParsePretty a => Text -> Maybe a
parsePretty = parseMaybe $ parsePrettyP @_ @Void

unsafeParsePretty :: ParsePretty a => Text -> a
unsafeParsePretty txt = fromMaybe (terror $ "Not a valid value: " <> txt) . parsePretty $ txt
