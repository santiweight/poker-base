{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Poker.BigBlind where

import Money (UnitScale, CurrencyScale, Discrete, Discrete', denseFromDiscrete, Dense)
import GHC.Generics (Generic)

type instance UnitScale "BB" "bb" = '(100, 1)

type instance CurrencyScale "BB" = UnitScale "BB" "bb"

type DiscreteDef curr = Discrete' curr (CurrencyScale curr)

newtype BigBlind = MkBigBlind { _unBigBlind :: Discrete "BB" "bb" }
  deriving (Show, Generic, Eq, Ord, Num, Fractional, Real)

bigBlindToDense :: BigBlind -> Dense "BB"
bigBlindToDense = denseFromDiscrete . _unBigBlind