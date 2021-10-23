module Poker.Utils where

enumerate :: (Enum a, Bounded a) => [a]
enumerate = [minBound .. maxBound]