{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ViewPatterns #-}
module Poker.Types.ActionIx where

import Data.Data
import GHC.Generics
import Algebra.PartialOrd (PartialOrd(leq))
import Poker.Types.Game
import Algebra.Lattice.Ordered (Ordered(Ordered, getOrdered))
import Control.Applicative (Applicative(liftA2))

data ActionIx b
  = AnyIx
  | RaiseIx (IxRange b)
  | AllInIx (IxRange b)
  | BetIx (IxRange b)
  | RaiseOrAllInIx (IxRange b)
  | CheckIx
  | CallIx
  | FoldIx
  | LeaveIx
  deriving (Show, Eq, Data, Typeable, Generic, Functor)

class IsBetSize b where
  plus :: b -> b -> b
  sub :: b -> b -> b
  times :: b -> b -> b
  empty :: b
  below :: b -> IxRange b -> Bool
  above :: b -> IxRange b -> Bool
  within :: b -> IxRange b -> Bool
  toPotSizeRelative :: PotSize b -> b -> b

deriving instance IsBetSize t => IsBetSize (PotSize t)

-- instance (Ord b, IsBetSize b) => IsBetSize (Ordered b) where
--   plus = liftA2 plus
--   sub = liftA2 sub
--   empty = Ordered empty

{-
>>> 0.1 + 0.2
0.30000000000000004
>>> flip roundTo 2 $ 0.1 + 0.2
0.3
-}
instance IsBetSize (Ordered Double) where
  plus = liftA2 (\l r -> roundToDecs 2 $ l + r)
  sub = liftA2 (\l r -> roundToDecs 2 $ l - r)
  times = liftA2 (\l r -> roundToDecs 2 $ l * r)
  empty = Ordered 0
  below = undefined
  above = undefined
  toPotSizeRelative = undefined
  within (getOrdered->l) (fmap getOrdered->r) = inRange l r

{-
>>> roundTo
-}

roundToDecs :: (RealFrac a, Fractional a) => Int -> a -> a
roundToDecs places num = fromInteger (round (num * (10 ^ places))) / (10 ^ places)

instance (Ord a, Num a) => IsBetSize (IxRange a) where
  plus = addRange
  sub = subRange
  empty = exactlyRn 0
  within = undefined
  below = undefined
  above = undefined
  times = undefined
  toPotSizeRelative = undefined
  -- within = inIndex


  -- toPotSizeRelative (PotSize potSize) betSize = betSize / potSize

data IxRange a = AnyRn | BetweenRn a a | AboveRn a | BelowRn a | ExactlyRn a
  deriving (Show, Eq, Data, Typeable, Generic, Functor)

instance Ord a => PartialOrd (IxRange a) where
  leq = undefined

exactlyRn :: a -> IxRange a
exactlyRn = ExactlyRn

anyRn :: IxRange a
anyRn = AnyRn

inRange :: Double -> IxRange Double -> Bool
inRange bet (BetweenRn low up) = low <= bet && bet <= up
inRange bet (ExactlyRn amount) = bet == amount
inRange bet (AboveRn low) = low <= bet
inRange bet (BelowRn up) = bet <= up
inRange _ AnyRn = True

addRange :: Num a => IxRange a -> IxRange a -> IxRange a
addRange AnyRn _     = AnyRn
addRange _     AnyRn = AnyRn
addRange (ExactlyRn amount1) (ExactlyRn amount2) = AboveRn $ amount1 + amount2
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
subRange (ExactlyRn amount1) (ExactlyRn amount2) = AboveRn $ amount1 - amount2
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
