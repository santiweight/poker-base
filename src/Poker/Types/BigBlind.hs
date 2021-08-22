{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Poker.Types.BigBlind where

import           GHC.Generics                   ( Generic )
import           Money                          ( CurrencyScale
                                                , Dense
                                                , Discrete
                                                , UnitScale
                                                , denseFromDiscrete, discrete, Discrete', GoodScale
                                                )
import GHC.TypeLits (Symbol, KnownSymbol)

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

newtype BigBlind = MkBigBlind { _unBigBlind :: Discrete "BB" "bb" }
  deriving (Show, Generic, Eq, Ord, Num, Fractional, Real)

data Amount (b :: Symbol) where
  Amount :: (GoodScale (CurrencyScale b), KnownSymbol b) => Discrete' b (CurrencyScale b) -> Amount b

deriving instance Show (Amount b)
deriving instance Eq (Amount b)
deriving instance Ord (Amount b)

-- TODO :)
instance (GoodScale (CurrencyScale b), KnownSymbol b) => Num (Amount b) where
  (Amount dis) + (Amount dis') = Amount $ dis + dis'
  (Amount dis) * (Amount dis') = Amount $ dis * dis'
  abs = undefined
  signum = undefined
  fromInteger int = Amount . discrete $ int
  negate (Amount dis) = Amount $ negate dis

class SmallAmount a where
  smallestAmount :: a

instance SmallAmount BigBlind where
  smallestAmount = smallestAmountBigBlind

instance (GoodScale (CurrencyScale b), KnownSymbol b) => SmallAmount (Amount b) where
  smallestAmount = Amount $ discrete 1 :: Amount b

bigBlindToDense :: BigBlind -> Dense "BB"
bigBlindToDense = denseFromDiscrete . _unBigBlind

smallestAmountBigBlind :: BigBlind
smallestAmountBigBlind = MkBigBlind $ discrete 1
