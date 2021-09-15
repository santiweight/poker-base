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
import Prettyprinter (Pretty (pretty), viaShow)

data Amount (b :: Symbol) where
  Amount :: (GoodScale (CurrencyScale b), KnownSymbol b) => { _unAmount :: Discrete' b (CurrencyScale b) } -> Amount b

deriving instance Show (Amount b)
deriving instance Eq (Amount b)
deriving instance Ord (Amount b)
instance Pretty (Amount b) where
  pretty = viaShow

class (Show b, Ord b, Monoid b) => IsBet b where
  smallestAmount :: b
  minus :: b -> b -> Maybe b

add :: IsBet b => b -> b -> b
add = (<>)

instance Semigroup (Amount b) where
  (Amount dis) <> (Amount dis') = Amount $ dis + dis'

instance (GoodScale (CurrencyScale b), KnownSymbol b) => Monoid (Amount b) where
  mempty = Amount $ discrete 0

instance (GoodScale (CurrencyScale b), KnownSymbol b) => IsBet (Amount b) where
  smallestAmount = Amount $ discrete 1 :: Amount b
  Amount l `minus` Amount r | r > l     = Nothing
                            | otherwise = Just $ Amount $ l - r
