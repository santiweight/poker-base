
{-# LANGUAGE DerivingVia #-}
-- {-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE AllowAmbiguousTypes #-}

module Poker.Types.ActionIx where

import Data.Data ( Data, Typeable )
import GHC.Generics ( Generic )
import Algebra.PartialOrd (PartialOrd(leq))
import Poker.Types.Game (BetAction)

data ActionIx b
  = MatchesBet (BetAction (IxRange b))
  | AnyIx
  | RaiseIx (IxRange b)
  | AllInIx (IxRange b)
  | BetIx (IxRange b)
  | RaiseOrAllInIx (IxRange b)
  | CheckIx
  | CallIx
  | FoldIx
  | LeaveIx
  deriving (Show, Eq, Functor)


type IsBetSize b = (Ord b, Num b)

within :: Ord a => a -> IxRange a -> Bool
within = inRange

data IxRange a = AnyRn | BetweenRn a a | AboveRn a | BelowRn a | ExactlyRn a
  deriving (Show, Eq, Data, Typeable, Generic, Functor)

instance (Ord a, Num a) => PartialOrd (IxRange a) where
  (ExactlyRn l) `leq` AnyRn = l == 0
  (BelowRn l) `leq` AnyRn = l == 0
  (AboveRn _) `leq` AnyRn = False
  (BetweenRn _ l) `leq` AnyRn = l == 0
  AnyRn `leq` _ = False
  (ExactlyRn l) `leq` (ExactlyRn r) = l <= r
  (ExactlyRn l) `leq` (BetweenRn r _) = l <= r
  (ExactlyRn l) `leq` (BelowRn _) = l == 0
  (ExactlyRn l) `leq` (AboveRn r) = l <= r
  (BelowRn l) `leq` (ExactlyRn r) = l <= r
  (BelowRn l) `leq` (BetweenRn r _) = l <= r
  (BelowRn l) `leq` (BelowRn _) = l == 0
  (BelowRn l) `leq` (AboveRn r) = l <= r
  (BetweenRn _ l) `leq` (ExactlyRn r) = l <= r
  (BetweenRn _ l) `leq` (BetweenRn r _) = l <= r
  (BetweenRn _ l) `leq` (BelowRn _) = l == 0
  (BetweenRn _ l) `leq` (AboveRn r) = l <= r
  (AboveRn l) `leq` (ExactlyRn r) = l <= r
  (AboveRn l) `leq` (BetweenRn r _) = l <= r
  (AboveRn _) `leq` (BelowRn _) = False
  (AboveRn l) `leq` (AboveRn r) = l <= r

exactlyRn :: a -> IxRange a
exactlyRn = ExactlyRn

anyRn :: IxRange a
anyRn = AnyRn

inRange :: Ord a => a -> IxRange a -> Bool
inRange bet (BetweenRn low up) = low <= bet && bet <= up
inRange bet (ExactlyRn amount) = bet == amount
inRange bet (AboveRn low) = low <= bet
inRange bet (BelowRn up) = bet <= up
inRange _ AnyRn = True

instance Num a => Num (IxRange a) where
  (+) = addRange
  (*) = undefined
  abs = fmap abs
  signum = fmap signum
  fromInteger = ExactlyRn . fromInteger
  negate = fmap negate

addRange :: Num a => IxRange a -> IxRange a -> IxRange a
addRange AnyRn _     = AnyRn
addRange _     AnyRn = AnyRn
addRange (ExactlyRn amount1) (ExactlyRn amount2) = ExactlyRn $ amount1 + amount2
addRange (ExactlyRn amount) (BetweenRn l u) = BetweenRn (l + amount) (u + amount)
addRange (ExactlyRn amount) (BelowRn bel) = BetweenRn amount (bel + amount)
addRange (ExactlyRn amount) (AboveRn ab) = AboveRn $ amount + ab
addRange (BetweenRn l u) (ExactlyRn amount) = BetweenRn (l + amount) (u + amount)
addRange (BetweenRn l1 u1) (BetweenRn l2 u2) = BetweenRn (l1 + l2) (u1 + u2)
addRange (BetweenRn l _) (AboveRn ab) = AboveRn (l + ab)
addRange (BetweenRn l u) (BelowRn bel) = BetweenRn l (u + bel)
addRange (BelowRn bel) (ExactlyRn amount) = BetweenRn amount (bel + amount)
addRange (BelowRn bel) (BetweenRn l u) = BetweenRn l (u + bel)
addRange (BelowRn _    ) (AboveRn ab) = AboveRn ab
addRange (BelowRn bel1) (BelowRn bel2) = BelowRn $ bel1 + bel2
addRange (AboveRn ab) (ExactlyRn amount) = AboveRn $ ab + amount
addRange (AboveRn ab) (BetweenRn l _) = AboveRn (l + ab)
addRange (AboveRn ab) (BelowRn _    ) = AboveRn ab
addRange (AboveRn ab1) (AboveRn ab2) = AboveRn $ ab1 + ab2

subRange :: Num a => IxRange a -> IxRange a -> IxRange a
subRange AnyRn _     = AnyRn
subRange _     AnyRn = AnyRn
subRange (ExactlyRn amount1) (ExactlyRn amount2) = ExactlyRn $ amount1 - amount2
subRange (ExactlyRn amount) (BetweenRn l u) = BetweenRn (l - amount) (u - amount)
subRange (ExactlyRn amount) (BelowRn bel) = BetweenRn amount (bel - amount)
subRange (ExactlyRn amount) (AboveRn ab) = AboveRn $ amount - ab
subRange (BetweenRn l1 u1) (BetweenRn l2 u2) =
  BetweenRn (l1 - u2) (u1 - l2)
subRange (BetweenRn l u) (ExactlyRn amount) = BetweenRn (l - amount) (u - amount)
subRange (BetweenRn _ u ) (AboveRn ab ) = BelowRn (u - ab)
subRange (BetweenRn l u ) (BelowRn bel ) = BetweenRn l (u - bel)
subRange (BelowRn bel ) (ExactlyRn amount) = BelowRn (bel - amount)
subRange (BelowRn bel ) (BetweenRn l u ) = BetweenRn l (u - bel)
subRange (BelowRn _     ) (AboveRn ab ) = AboveRn ab
subRange (BelowRn bel1) (BelowRn _     ) = BelowRn bel1
subRange (AboveRn ab ) (ExactlyRn amount) = AboveRn (ab - amount)
subRange (AboveRn ab ) (BetweenRn l u ) = BetweenRn (l - ab) u
subRange (AboveRn ab ) (BelowRn _     ) = AboveRn ab
subRange (AboveRn above1) (AboveRn above2) = AboveRn $ above1 - above2

-- getRelativePotSize :: forall a. (a -> a -> a) -> IxRange a -> IxRange a -> IxRange (Ratio a)
-- getRelativePotSize div (ixRange -> l) (ixRange -> r) = IxRange $ go l r
--   where
--     go :: Range a -> Range a -> Range a
--     go AnyRn _     = AnyRn
--     go _     AnyRn = AnyRn
--     go (ExactlyRn amount1) (ExactlyRn amount2) = ExactlyRn $ amount1 % amount2
--     go (ExactlyRn amount) (BetweenRn l u) = BetweenRn (l - amount) (u - amount)
--     go (ExactlyRn amount) (BelowRn below) = BetweenRn amount (below - amount)
--     go (ExactlyRn amount) (AboveRn above) = AboveRn $ amount - above
--     go (BetweenRn l1 u1) (BetweenRn l2 u2) =
--       BetweenRn (l1 - u2) (u1 - l2)
--     go (BetweenRn l u) (ExactlyRn amount) = BetweenRn (l - amount) (u - amount)
--     go (BetweenRn _ u ) (AboveRn above ) = BelowRn (u - above)
--     go (BetweenRn l u ) (BelowRn below ) = BetweenRn l (u - below)
--     go (BelowRn below ) (ExactlyRn amount) = BelowRn (below - amount)
--     go (BelowRn below ) (BetweenRn l u ) = BetweenRn l (u - below)
--     go (BelowRn _     ) (AboveRn above ) = AboveRn above
--     go (BelowRn below1) (BelowRn _     ) = BelowRn below1
--     go (AboveRn above ) (ExactlyRn amount) = AboveRn (above - amount)
--     go (AboveRn above ) (BetweenRn l u ) = BetweenRn (l - above) u
--     go (AboveRn above ) (BelowRn _     ) = AboveRn above
--     go (AboveRn above1) (AboveRn above2) = AboveRn $ above1 - above2
