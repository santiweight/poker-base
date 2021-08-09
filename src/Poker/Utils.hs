module Poker.Utils where

import Data.Text (Text)
import qualified Data.Text as T
import Text.Megaparsec (MonadParsec)

terror :: Text -> a
terror = error . T.unpack

tfail :: (MonadParsec e s m, MonadFail m) => Text ->  m a
tfail = fail . T.unpack

atMay :: [a] -> Int -> Maybe a
atMay xs i
    | i < 0     = Nothing
    | otherwise = go i xs
  where
    go :: Int -> [a] -> Maybe a
    go 0 (x:_)  = Just x
    go j (_:ys) = go (j - 1) ys
    go _ []     = Nothing
{-# INLINE atMay #-}