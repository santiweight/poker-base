module Poker.Types.ActionIx where

import Data.Data
import GHC.Generics

-- | An index for numbers indicates the range of accepted values
data IxRange t
  = AnyRn
  | BetweenRn t t
  | AboveRn t
  | BelowRn t
  | ExactlyRn t
  deriving (Show, Eq, Ord, Data, Typeable, Generic)

-- | An index for matching 'PlayerAction's
data ActionIx t
  = AnyIx
  | RaiseIx (IxRange t)
  | AllInIx (IxRange t)
  | BetIx (IxRange t)
  | RaiseOrAllInIx (IxRange t)
  | CheckIx
  | CallIx
  | FoldIx
  | LeaveIx
  deriving (Show, Eq, Ord, Data, Typeable, Generic)

