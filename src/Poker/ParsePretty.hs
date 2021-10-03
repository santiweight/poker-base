{-# LANGUAGE OverloadedStrings #-}

module Poker.ParsePretty where

import Text.Megaparsec (Parsec, parseMaybe, MonadParsec, (<?>), empty)
import Data.Void (Void)
import Data.Text (Text)
import Data.Maybe (fromMaybe)
import Poker.Utils (terror)
import qualified Data.Text as T

type PrettyParser a = Parsec Void Text a

class ParsePretty a where
  parsePrettyP :: Ord e => MonadParsec e Text m => m a

tfailure :: MonadParsec e Text m => Text -> m a
tfailure err = empty <?> T.unpack err

parsePretty :: ParsePretty a => Text -> Maybe a
parsePretty = parseMaybe $ parsePrettyP @_ @Void

unsafeParsePretty :: ParsePretty a => Text -> a
unsafeParsePretty txt = fromMaybe (terror $ "Not a valid value: " <> txt) . parsePretty $ txt
