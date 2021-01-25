module Poker.Types.IsBetSize where

import Poker.Types.ActionIx
import Poker.Types.Game (PotSize)
import Algebra.Lattice.Ordered (Ordered(getOrdered, Ordered))
import Control.Applicative (Applicative(liftA2))

class IsBetSize b where
  plus :: b -> b -> b
  sub :: b -> b -> b
  empty :: b
  below :: b -> IxRange b -> Bool
  above :: b -> IxRange b -> Bool
  within :: b -> IxRange b -> Bool
  toPotSizeRelative :: PotSize b -> b -> b


instance IsBetSize b => IsBetSize (Ordered b) where
  plus = liftA2 plus
  sub = liftA2 sub
  empty = Ordered empty

instance IsBetSize Double where
  plus = (+)
  sub = (-)
  empty = 0

instance (Ord a, Num a) => IsBetSize (IxRange a) where
  plus = addRange
  sub = subRange
  empty = exactlyRn 0


  -- toPotSizeRelative (PotSize potSize) betSize = betSize / potSize
