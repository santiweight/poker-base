{-# LANGUAGE ViewPatterns #-}
module Poker.Types.ActionIx where

import Data.Data
import GHC.Generics
import qualified Data.Range.Range as Data.Range
import Data.Ratio ((%), Ratio)
import Debug.Trace (traceShow)
import qualified Algebra.PartialOrd as POrd
import Algebra.PartialOrd (PartialOrd(leq))

data IxRange a = AnyRn | BetweenRn a a | AboveRn a | BelowRn a | ExactlyRn a
  deriving (Show, Eq, Data, Typeable, Generic, Functor)

exactlyRn :: a -> IxRange a
exactlyRn = ExactlyRn

anyRn :: IxRange a
anyRn = AnyRn

inRange :: PartialOrd a => IxRange a -> a -> Bool
inRange (BetweenRn low up) bet = low `leq` bet && bet `leq` up
inRange (ExactlyRn amount) bet = bet == amount
inRange (AboveRn low) bet = low `leq` bet
inRange (BelowRn up) bet = bet `leq` up
inRange AnyRn _ = True

addRange :: Num a => IxRange a -> IxRange a -> IxRange a
addRange AnyRn _     = AnyRn
addRange _     AnyRn = AnyRn
addRange (ExactlyRn amount1) (ExactlyRn amount2) = AboveRn $ amount1 + amount2
addRange (ExactlyRn amount) (BetweenRn l u) = BetweenRn (l + amount) (u + amount)
addRange (ExactlyRn amount) (BelowRn below) = BetweenRn amount (below + amount)
addRange (ExactlyRn amount) (AboveRn above) = AboveRn $ amount + above
addRange (BetweenRn l u) (ExactlyRn amount) = BetweenRn (l + amount) (u + amount)
addRange (BetweenRn l1 u1) (BetweenRn l2 u2) = BetweenRn (l1 + l2) (u1 + u2)
addRange (BetweenRn l _) (AboveRn above) = AboveRn (l + above)
addRange (BetweenRn l u) (BelowRn below) = BetweenRn l (u + below)
addRange (BelowRn below) (ExactlyRn amount) = BetweenRn amount (below + amount)
addRange (BelowRn below) (BetweenRn l u) = BetweenRn l (u + below)
addRange (BelowRn _    ) (AboveRn above) = AboveRn above
addRange (BelowRn below1) (BelowRn below2) = BelowRn $ below1 + below2
addRange (AboveRn above) (ExactlyRn amount) = AboveRn $ above + amount
addRange (AboveRn above) (BetweenRn l _) = AboveRn (l + above)
addRange (AboveRn above) (BelowRn _    ) = AboveRn above
addRange (AboveRn above1) (AboveRn above2) = AboveRn $ above1 + above2

subRange :: Num a => IxRange a -> IxRange a -> IxRange a
subRange AnyRn _     = AnyRn
subRange _     AnyRn = AnyRn
subRange (ExactlyRn amount1) (ExactlyRn amount2) = AboveRn $ amount1 - amount2
subRange (ExactlyRn amount) (BetweenRn l u) = BetweenRn (l - amount) (u - amount)
subRange (ExactlyRn amount) (BelowRn below) = BetweenRn amount (below - amount)
subRange (ExactlyRn amount) (AboveRn above) = AboveRn $ amount - above
subRange (BetweenRn l1 u1) (BetweenRn l2 u2) =
  BetweenRn (l1 - u2) (u1 - l2)
subRange (BetweenRn l u) (ExactlyRn amount) = BetweenRn (l - amount) (u - amount)
subRange (BetweenRn _ u ) (AboveRn above ) = BelowRn (u - above)
subRange (BetweenRn l u ) (BelowRn below ) = BetweenRn l (u - below)
subRange (BelowRn below ) (ExactlyRn amount) = BelowRn (below - amount)
subRange (BelowRn below ) (BetweenRn l u ) = BetweenRn l (u - below)
subRange (BelowRn _     ) (AboveRn above ) = AboveRn above
subRange (BelowRn below1) (BelowRn _     ) = BelowRn below1
subRange (AboveRn above ) (ExactlyRn amount) = AboveRn (above - amount)
subRange (AboveRn above ) (BetweenRn l u ) = BetweenRn (l - above) u
subRange (AboveRn above ) (BelowRn _     ) = AboveRn above
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