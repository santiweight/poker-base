module Poker.Types.IsBetSize where

import Poker.Types.ActionIx (IxRange(..))
import Poker.Types.Game (PotSize(PotSize))

class IsBetSize b where
  plus :: b -> b -> b
  sub :: b -> b -> b
  empty :: b
  inRange :: IxRange b -> b -> Bool
  toPotSizeRelative :: PotSize b -> b -> b



instance IsBetSize Double where
  plus = (+)
  sub = (-)
  empty = 0

  inRange (BetweenRn low up) bet = low < bet && bet < up
  inRange (ExactlyRn amount) bet = bet == amount
  inRange (AboveRn low) bet = low < bet
  inRange (BelowRn up) bet = bet < up
  inRange AnyRn _ = True

  toPotSizeRelative (PotSize potSize) betSize = betSize / potSize

instance IsBetSize b => IsBetSize (IxRange b) where
  empty = ExactlyRn empty

  plus AnyRn _     = AnyRn
  plus _     AnyRn = AnyRn
  plus (ExactlyRn amount1) (ExactlyRn amount2) = AboveRn $ amount1 `plus` amount2
  plus (ExactlyRn amount) (BetweenRn l u) = BetweenRn (l `plus` amount) (u `plus` amount)
  plus (ExactlyRn amount) (BelowRn below) = BetweenRn amount (below `plus` amount)
  plus (ExactlyRn amount) (AboveRn above) = AboveRn $ amount `plus` above
  plus (BetweenRn l u) (ExactlyRn amount) = BetweenRn (l `plus` amount) (u `plus` amount)
  plus (BetweenRn l1 u1) (BetweenRn l2 u2) =
    BetweenRn (l1 `plus` l2) (u1 `plus` u2)
  plus (BetweenRn l _) (AboveRn above) = AboveRn (l `plus` above)
  plus (BetweenRn l u) (BelowRn below) = BetweenRn l (u `plus` below)
  plus (BelowRn below) (ExactlyRn amount) = BetweenRn amount (below `plus` amount)
  plus (BelowRn below) (BetweenRn l u) = BetweenRn l (u `plus` below)
  plus (BelowRn _    ) (AboveRn above) = AboveRn above
  plus (BelowRn below1) (BelowRn below2) = BelowRn $ below1 `plus` below2
  plus (AboveRn above) (ExactlyRn amount) = AboveRn $ above `plus` amount
  plus (AboveRn above) (BetweenRn l _) = AboveRn (l `plus` above)
  plus (AboveRn above) (BelowRn _    ) = AboveRn above
  plus (AboveRn above1) (AboveRn above2) = AboveRn $ above1 `plus` above2

  sub AnyRn _     = AnyRn
  sub _     AnyRn = AnyRn
  sub (BetweenRn l1 u1) (BetweenRn l2 u2) =
    BetweenRn (l1 `sub` u2) (u1 `sub` l2)
  sub (BetweenRn _ u ) (AboveRn above ) = BelowRn (u `sub` above)
  sub (BetweenRn l u ) (BelowRn below ) = BetweenRn l (u `sub` below)
  sub (BelowRn below ) (BetweenRn l u ) = BetweenRn l (u `sub` below)
  sub (BelowRn _     ) (AboveRn above ) = AboveRn above
  sub (BelowRn below1) (BelowRn _     ) = BelowRn below1
  sub (AboveRn above ) (BetweenRn l u ) = BetweenRn (l `sub` above) u
  sub (AboveRn above ) (BelowRn _     ) = AboveRn above
  sub (AboveRn above1) (AboveRn above2) = AboveRn $ above1 `sub` above2

  inRange (BetweenRn low up) bet = undefined

  toPotSizeRelative = undefined