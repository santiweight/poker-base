module Poker.Pretty where


import Prettyprinter (Pretty (pretty), layoutCompact)
import Data.Text (Text)
import Prettyprinter.Render.Text (renderStrict)

prettyText :: Pretty a => a -> Text
prettyText = renderStrict . layoutCompact . pretty
