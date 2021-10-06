{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE CPP #-}

module Poker.ParsePretty where

import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Void (Void)
import Poker.Utils (terror)
import Text.Megaparsec (MonadParsec, empty, parseMaybe, (<?>))
#if MIN_VERSION_prettyprinter(1,7,0)
import Prettyprinter (Pretty)
#else
import Data.Text.Prettyprint.Doc
#endif

-- | A class for parsing the output of the 'Pretty' instance for @a@.
class Pretty a => ParsePretty a where
  parsePrettyP :: Ord e => MonadParsec e Text m => m a

tfailure :: MonadParsec e Text m => Text -> m a
tfailure err = empty <?> T.unpack err

parsePretty :: ParsePretty a => Text -> Maybe a
parsePretty = parseMaybe $ parsePrettyP @_ @Void

unsafeParsePretty :: ParsePretty a => Text -> a
unsafeParsePretty txt = fromMaybe (terror $ "Not a valid value: " <> txt) . parsePretty $ txt
