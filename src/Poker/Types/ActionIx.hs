{-# LANGUAGE ViewPatterns #-}
module Poker.Types.ActionIx where

import Data.Data
import GHC.Generics
import qualified Data.Range.Range as Data.Range
import Data.Range.Range (Range(UpperBoundRange, LowerBoundRange, SpanRange, SingletonRange, InfiniteRange))
import Data.Ratio ((%), Ratio)
import Debug.Trace (traceShow)

-- data IxRange b
--   = InfiniteRange
--   | SingletonRange b
--   | SpanRange b b
--   | LowerBoundRange b
--   | UpperBoundRange b
--   deriving (Show, Eq, Ord, Data, Typeable, Generic)

deriving instance Data a => Data (Data.Range.Range a)
deriving instance Generic (Data.Range.Range a)

newtype IxRange a = IxRange {ixRange :: Data.Range.Range a}
  deriving (Show, Eq, Data, Typeable, Generic)

exactlyRn :: a -> IxRange a
exactlyRn = IxRange . SingletonRange

anyRn :: IxRange a
anyRn = IxRange InfiniteRange

inRange :: Ord a => IxRange a -> a -> Bool
inRange = Data.Range.inRange . ixRange

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
  deriving (Show, Eq, Data, Typeable, Generic)


addRange :: forall a . (a -> a -> a) -> IxRange a -> IxRange a -> IxRange a
addRange plus (ixRange -> l) (ixRange -> r) = IxRange $ go l r
  where
    go :: Range a -> Range a -> Range a
    go InfiniteRange _     = InfiniteRange
    go _     InfiniteRange = InfiniteRange
    go (SingletonRange amount1) (SingletonRange amount2) = LowerBoundRange $ amount1 `plus` amount2
    go (SingletonRange amount) (SpanRange l u) = SpanRange (l `plus` amount) (u `plus` amount)
    go (SingletonRange amount) (UpperBoundRange below) = SpanRange amount (below `plus` amount)
    go (SingletonRange amount) (LowerBoundRange above) = LowerBoundRange $ amount `plus` above
    go (SpanRange l u) (SingletonRange amount) = SpanRange (l `plus` amount) (u `plus` amount)
    go (SpanRange l1 u1) (SpanRange l2 u2) = SpanRange (l1 `plus` l2) (u1 `plus` u2)
    go (SpanRange l _) (LowerBoundRange above) = LowerBoundRange (l `plus` above)
    go (SpanRange l u) (UpperBoundRange below) = SpanRange l (u `plus` below)
    go (UpperBoundRange below) (SingletonRange amount) = SpanRange amount (below `plus` amount)
    go (UpperBoundRange below) (SpanRange l u) = SpanRange l (u `plus` below)
    go (UpperBoundRange _    ) (LowerBoundRange above) = LowerBoundRange above
    go (UpperBoundRange below1) (UpperBoundRange below2) = UpperBoundRange $ below1 `plus` below2
    go (LowerBoundRange above) (SingletonRange amount) = LowerBoundRange $ above `plus` amount
    go (LowerBoundRange above) (SpanRange l _) = LowerBoundRange (l `plus` above)
    go (LowerBoundRange above) (UpperBoundRange _    ) = LowerBoundRange above
    go (LowerBoundRange above1) (LowerBoundRange above2) = LowerBoundRange $ above1 `plus` above2

subRange :: forall a . (a -> a -> a) -> IxRange a -> IxRange a -> IxRange a
subRange sub (ixRange -> l) (ixRange -> r) = IxRange $ go l r
  where
    go :: Range a -> Range a -> Range a
    go InfiniteRange _     = InfiniteRange
    go _     InfiniteRange = InfiniteRange
    go (SingletonRange amount1) (SingletonRange amount2) = LowerBoundRange $ amount1 `sub` amount2
    go (SingletonRange amount) (SpanRange l u) = SpanRange (l `sub` amount) (u `sub` amount)
    go (SingletonRange amount) (UpperBoundRange below) = SpanRange amount (below `sub` amount)
    go (SingletonRange amount) (LowerBoundRange above) = LowerBoundRange $ amount `sub` above
    go (SpanRange l1 u1) (SpanRange l2 u2) =
      SpanRange (l1 `sub` u2) (u1 `sub` l2)
    go (SpanRange l u) (SingletonRange amount) = SpanRange (l `sub` amount) (u `sub` amount)
    go (SpanRange _ u ) (LowerBoundRange above ) = UpperBoundRange (u `sub` above)
    go (SpanRange l u ) (UpperBoundRange below ) = SpanRange l (u `sub` below)
    go (UpperBoundRange below ) (SingletonRange amount) = UpperBoundRange (below `sub` amount)
    go (UpperBoundRange below ) (SpanRange l u ) = SpanRange l (u `sub` below)
    go (UpperBoundRange _     ) (LowerBoundRange above ) = LowerBoundRange above
    go (UpperBoundRange below1) (UpperBoundRange _     ) = UpperBoundRange below1
    go (LowerBoundRange above ) (SingletonRange amount) = LowerBoundRange (above `sub` amount)
    go (LowerBoundRange above ) (SpanRange l u ) = SpanRange (l `sub` above) u
    go (LowerBoundRange above ) (UpperBoundRange _     ) = LowerBoundRange above
    go (LowerBoundRange above1) (LowerBoundRange above2) = LowerBoundRange $ above1 `sub` above2

-- getRelativePotSize :: forall a. (a -> a -> a) -> IxRange a -> IxRange a -> IxRange (Ratio a)
-- getRelativePotSize div (ixRange -> l) (ixRange -> r) = IxRange $ go l r
--   where
--     go :: Range a -> Range a -> Range a
--     go InfiniteRange _     = InfiniteRange
--     go _     InfiniteRange = InfiniteRange
--     go (SingletonRange amount1) (SingletonRange amount2) = SingletonRange $ amount1 % amount2
--     go (SingletonRange amount) (SpanRange l u) = SpanRange (l `sub` amount) (u `sub` amount)
--     go (SingletonRange amount) (UpperBoundRange below) = SpanRange amount (below `sub` amount)
--     go (SingletonRange amount) (LowerBoundRange above) = LowerBoundRange $ amount `sub` above
--     go (SpanRange l1 u1) (SpanRange l2 u2) =
--       SpanRange (l1 `sub` u2) (u1 `sub` l2)
--     go (SpanRange l u) (SingletonRange amount) = SpanRange (l `sub` amount) (u `sub` amount)
--     go (SpanRange _ u ) (LowerBoundRange above ) = UpperBoundRange (u `sub` above)
--     go (SpanRange l u ) (UpperBoundRange below ) = SpanRange l (u `sub` below)
--     go (UpperBoundRange below ) (SingletonRange amount) = UpperBoundRange (below `sub` amount)
--     go (UpperBoundRange below ) (SpanRange l u ) = SpanRange l (u `sub` below)
--     go (UpperBoundRange _     ) (LowerBoundRange above ) = LowerBoundRange above
--     go (UpperBoundRange below1) (UpperBoundRange _     ) = UpperBoundRange below1
--     go (LowerBoundRange above ) (SingletonRange amount) = LowerBoundRange (above `sub` amount)
--     go (LowerBoundRange above ) (SpanRange l u ) = SpanRange (l `sub` above) u
--     go (LowerBoundRange above ) (UpperBoundRange _     ) = LowerBoundRange above
--     go (LowerBoundRange above1) (LowerBoundRange above2) = LowerBoundRange $ above1 `sub` above2