{-# LANGUAGE UndecidableInstances #-}

module Poker.Amount where

import           GHC.TypeLits                   ( KnownSymbol
                                                , Symbol
                                                )
import           Money                          ( CurrencyScale
                                                , Discrete'
                                                , GoodScale
                                                , discrete
                                                )

data Amount (b :: Symbol) where
  Amount ::(GoodScale (CurrencyScale b), KnownSymbol b) => Discrete' b (CurrencyScale b) -> Amount b

deriving instance Show (Amount b)
deriving instance Eq (Amount b)
deriving instance Ord (Amount b)

-- TODO :)
instance (GoodScale (CurrencyScale b), KnownSymbol b) => Num (Amount b) where
  (Amount dis) + (Amount dis') = Amount $ dis + dis'
  (Amount dis) * (Amount dis') = Amount $ dis * dis'
  abs    = undefined
  signum = undefined
  fromInteger int = Amount . discrete $ int
  negate (Amount dis) = Amount $ negate dis

class SmallAmount a where
  smallestAmount :: a

instance (GoodScale (CurrencyScale b), KnownSymbol b) => SmallAmount (Amount b) where
  smallestAmount = Amount $ discrete 1 :: Amount b
