{-# LANGUAGE UndecidableInstances #-}

module Poker.BigBlind where

import           GHC.Generics                   ( Generic )
import           Money                          ( CurrencyScale
                                                , Dense
                                                , UnitScale
                                                , denseFromDiscrete
                                                , discrete
                                                )
import           Poker.Amount

-- | Big blinds are encoded using the safe-money package.
-- Calculations in the safe-money package are done with Discrete and Dense
-- types. Discrete values are used to describe a regular BigBlind value,
-- such as 1.30bb. Dense values are used when calculating some complex
-- (non-discrete) value such as one third of a big blind.30bb. When using the BigBlind
-- type, it is best to do all calculation with Dense "BB" values and then
-- convert back to a Discrete "BB" "bb" after all calculation has been completed:
--
-- TODO add examples

type instance UnitScale "BB" "bb"
  = '(100, 1)

type instance CurrencyScale "BB" = UnitScale "BB" "bb"

newtype BigBlind = BigBlind { _unBigBlind :: Amount "BB" }
  deriving (Show, Generic, Eq, Ord, IsBet, Semigroup, Monoid)

bigBlindToDense :: BigBlind -> Dense "BB"
bigBlindToDense = denseFromDiscrete . _unAmount . _unBigBlind

smallestAmountBigBlind :: BigBlind
smallestAmountBigBlind = BigBlind . Amount $ discrete 1
