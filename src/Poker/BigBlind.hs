{-# LANGUAGE UndecidableInstances #-}

-- TODO fix exports
module Poker.BigBlind ( BigBlind(..),bigBlindToDense)where

import GHC.Generics (Generic)
import Money
import Poker.Amount

type instance
  UnitScale "BB" "bb" =
    '(100, 1)

type instance CurrencyScale "BB" = UnitScale "BB" "bb"

-- |
-- 'BigBlind' is the type describing poker chip amounts that are measured in big blinds.
--
-- The internal representation of 'BigBlind' is @'Amount' "BB"@. This module introduces
-- a new instance of 'CurrencyScale' (from the
-- <https://hackage.haskell.org/package/safe-money safe-money> package), which allows
-- translation from BigBlind to any valid currency in a lossless manner.
--
-- The small unit of a \"BB\" is a \"bb\", with 100 \"bb\"s in a \"BB\".
--
-- TODO include an API for translating from BigBlind to any safe-money currency, given
-- a 'Stake'.
--
-- Calculations in the safe-money package are done with Discrete and Dense
-- types. Discrete values are used to describe a regular BigBlind value,
-- such as 1.30bb. Dense values are used when calculating some complex
-- (non-discrete) value such as one third of a big blind. When using the BigBlind
-- type, it is best to do all calculation with Dense "BB" values and then
-- convert back to a Discrete "BB" "bb" after all calculation has been completed:
newtype BigBlind = BigBlind {unBigBlind :: Amount "BB"}
  deriving (Show, Generic, Eq, Ord, IsBet, Semigroup, Monoid)

-- | When working with a 'BigBlind' you might want to (cautiously) retain losslessness
-- when using functions such as % calculations or division. A 'Dense' allows you to do so.
bigBlindToDense :: BigBlind -> Dense "BB"
bigBlindToDense = denseFromDiscrete . unAmount . unBigBlind
