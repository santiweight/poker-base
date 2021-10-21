{-# LANGUAGE CPP #-}
{-# LANGUAGE UndecidableInstances #-}

module Poker.Amount
  ( Amount (unAmount),
    pattern Amount,
    unsafeMkAmount,
    IsBet (..),
    mkAmount,
  )
where

import GHC.TypeLits
  ( KnownSymbol,
    Symbol,
  )
import Money
  ( CurrencyScale,
    Discrete',
    GoodScale,
    discrete,
    someDiscreteAmount,
    toSomeDiscrete,
  )

#if MIN_VERSION_prettyprinter(1,7,0)
import Prettyprinter (Pretty (pretty), viaShow)
#else
import Data.Text.Prettyprint.Doc (Pretty (pretty), viaShow)
#endif

-- $setup
-- >>> :set -XDataKinds

-- |
-- 'Amount' is the type used to represent amounts of money during a game of poker.
-- The internal representation of 'Amount' is a @Discrete\'@ from the
-- <https://hackage.haskell.org/package/safe-money safe-money> package.
-- The exposed constructors for 'Amount' ensure that no 'Amount' can have a negative value.
--
-- The use of the safe-money package allows for lossless conversion between currencies with
-- well-maintained support for type safety, serialisation, and currency conversions.
--
-- TODO add examples section
data Amount (b :: Symbol) where
  UnsafeMkAmount :: (GoodScale (CurrencyScale b), KnownSymbol b) => {unAmount :: Discrete' b (CurrencyScale b)} -> Amount b

deriving instance Show (Amount b)

deriving instance Eq (Amount b)

deriving instance Ord (Amount b)

instance Pretty (Amount b) where
  pretty = viaShow

-- |
-- A pattern for accessing the @Discrete\'@ within an 'Amount'
--
-- @
-- \{\-\# Language TypeApplications \#\-\}
--
-- case 'unsafeMkAmount' @\"USD\" ('discrete' 100) of
--   'Amount' x -> x     -- x == discrete 100
-- @
{-# COMPLETE Amount #-}

pattern Amount ::
  (GoodScale (CurrencyScale b), KnownSymbol b) =>
  Discrete' b (CurrencyScale b) ->
  Amount b
pattern Amount x <- UnsafeMkAmount x

-- |
-- Returns an 'Amount' from a @Discrete\'@ so long as the given @Discrete\'@ is non-negative.
--
-- @
-- >>> mkAmount @"USD" 0
-- Just (UnsafeMkAmount {unAmount = Discrete "USD" 100%1 0})
-- >>> mkAmount @"USD" (-1)
-- Nothing
mkAmount ::
  (GoodScale (CurrencyScale b), KnownSymbol b) =>
  Discrete' b (CurrencyScale b) ->
  Maybe (Amount b)
mkAmount (someDiscreteAmount . toSomeDiscrete -> amt)
  | amt >= 0 = Just $ UnsafeMkAmount $ discrete amt
  | otherwise = Nothing

-- | Make an 'Amount' from a @Discrete'@. Only use when you are certain that your @Discrete'@ value
-- is positive, since most usages of 'Amount' will break for negative quantities.
unsafeMkAmount ::
  (GoodScale (CurrencyScale b), KnownSymbol b) =>
  Discrete' b (CurrencyScale b) ->
  Amount b
unsafeMkAmount = UnsafeMkAmount

-- |
-- A type @b@ satisfies 'IsBet' if we know:
--
--   * A 'Monoid' instance for @b@. This allows us to construct a zero amount of @b@ and
--     to 'add' two amounts of @b@ together.
--
--   * the smallest non-zero currency unit for @b@ ('smallestAmount'). For example, for USD the minimum currency amount
--     is $0.01.
--
--   * how to 'add' two @b@s. By default, this is the 'Monoid' instance's 'append' for @b@.
--
--   * how to 'minus' two @b@s, which may fail (returning 'Nothing'), if the resulting 'Amount' is negative.
--
-- Types that satisfy 'IsBet' are expected to have both 'Ord' and 'Show' instances, so that packages such as @poker-game@
-- can handle arbitrary new user bet types.
--
-- For an example instance of the 'IsBet' class, see "Poker.BigBlind".
class (Monoid b, Show b, Ord b) => IsBet b where
  smallestAmount :: b
  minus :: b -> b -> Maybe b
  add :: b -> b -> b
  add = (<>)

-- TODO There's probably some way to avoid repeating the Constraints in the typeclasses,
-- since they are implied by the constructor of Amount. However this might require some
-- work from Richard Eisenberg first...
instance (GoodScale (CurrencyScale b), KnownSymbol b) => Semigroup (Amount b) where
  (Amount dis) <> (Amount dis') = UnsafeMkAmount $ dis + dis'

instance (GoodScale (CurrencyScale b), KnownSymbol b) => Monoid (Amount b) where
  mempty = UnsafeMkAmount $ discrete 0

instance (GoodScale (CurrencyScale b), KnownSymbol b) => IsBet (Amount b) where
  smallestAmount = UnsafeMkAmount $ discrete 1 :: Amount b
  Amount l `minus` Amount r
    | r > l = Nothing
    | otherwise = Just $ UnsafeMkAmount $ l - r
  Amount l `add` Amount r = UnsafeMkAmount $ l + r
