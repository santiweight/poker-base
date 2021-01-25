module Poker.Types.IsBetSize where

import Poker.Types.ActionIx
import Poker.Types.Game (PotSize)

class IsBetSize b where
  plus :: b -> b -> b
  sub :: b -> b -> b
  empty :: b
  toPotSizeRelative :: PotSize b -> b -> b



instance IsBetSize Double where
  plus = (+)
  sub = (-)
  empty = 0

instance (Ord a, Num a) => IsBetSize (IxRange a) where
  plus = addRange (+)
  sub = subRange (-)
  empty = exactlyRn 0


  -- toPotSizeRelative (PotSize potSize) betSize = betSize / potSize
