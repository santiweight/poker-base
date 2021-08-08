module Poker.ParsePretty where

import Text.Megaparsec (Parsec, parseMaybe)
import Data.Void (Void)
import Data.Text (Text)
import Data.Maybe (fromJust)

type PrettyParser a = Parsec Void Text a

class ParsePretty a where
  parsePrettyP :: PrettyParser a

parsePretty :: ParsePretty a => Text -> Maybe a
parsePretty = parseMaybe parsePrettyP

unsafeParsePretty :: ParsePretty a => Text -> a
unsafeParsePretty = fromJust . parsePretty

