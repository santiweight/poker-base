module Poker.Types.ActionIx where

import Data.Data
import GHC.Generics

data IxRange
  = AnyRn
  | BetweenRn Double Double
  | AboveRn Double
  | BelowRn Double
  deriving (Show, Eq, Ord, Data, Typeable, Generic)

data ActionIx
  = AnyIx
  | RaiseIx IxRange
  | AllInIx IxRange
  | BetIx IxRange
  | RaiseOrAllInIx IxRange
  | CheckIx
  | CallIx
  | FoldIx
  | LeaveIx
  deriving (Show, Eq, Ord, Data, Typeable, Generic)

