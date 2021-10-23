{-# LANGUAGE CPP #-}
{-# LANGUAGE UndecidableInstances #-}

-- | Representation of money, and bet quantities.
module Poker.Amount
  ( Amount (..),
    unsafeAmount,
    IsBet (..),
    mkAmount,
    bigBlindToDense,
    BigBlind (..),
  )
where

import GHC.Generics (Generic)
import GHC.TypeLits
  ( KnownSymbol,
    Symbol,
  )
import Money

#if MIN_VERSION_prettyprinter(1,7,0)
import Prettyprinter (Pretty (pretty), viaShow)
#else
import Data.Text.Prettyprint.Doc (Pretty (pretty), viaShow)
#endif

-- $setup
-- >>> :set -XDataKinds
-- >>> import Prettyprinter

-- | 'Amount' is the type used to represent amounts of money during a game of poker.
-- The internal representation of 'Amount' is a @Discrete\'@ from the
-- <https://hackage.haskell.org/package/safe-money safe-money> package.
-- The exposed constructors for 'Amount' ensure that no 'Amount' can have a negative value.
--
-- The use of the @safe-money@ package allows for lossless conversion between currencies with
-- well-maintained support for type safety, serialisation, and currency conversions.
--
-- @
-- \{\-\# Language TypeApplications \#\-\}
--
-- case 'unsafeAmount' @\"USD\" ('discrete' 100) of
--   'UnsafeAmount' x -> x     -- x == discrete 100
-- @
data Amount (b :: Symbol) where
  UnsafeAmount :: (GoodScale (CurrencyScale b), KnownSymbol b) => {unAmount :: Discrete' b (CurrencyScale b)} -> Amount b

deriving instance Show (Amount b)

deriving instance Eq (Amount b)

deriving instance Ord (Amount b)

instance Pretty (Amount b) where
  pretty = viaShow

-- |
-- Returns an 'Amount' from a @Discrete\'@ so long as the given @Discrete\'@ is non-negative.
--
-- >>> mkAmount @"USD" 0
-- Just (UnsafeAmount {unAmount = Discrete "USD" 100%1 0})
-- >>> mkAmount @"USD" (-1)
-- Nothing
mkAmount ::
  (GoodScale (CurrencyScale b), KnownSymbol b) =>
  Discrete' b (CurrencyScale b) ->
  Maybe (Amount b)
mkAmount (someDiscreteAmount . toSomeDiscrete -> amt)
  | amt >= 0 = Just $ UnsafeAmount $ discrete amt
  | otherwise = Nothing

-- | Make an 'Amount' from a @Discrete'@. Only use when you are certain that your @Discrete'@ value
-- is positive, since most usages of 'Amount' will break for negative quantities.
unsafeAmount ::
  (GoodScale (CurrencyScale b), KnownSymbol b) =>
  Discrete' b (CurrencyScale b) ->
  Amount b
unsafeAmount = UnsafeAmount

-- |
-- A type @b@ satisfies 'IsBet' if we know:
--
--   * A 'Monoid' instance for @b@. This allows us to construct a zero amount of @b@ and
--     to 'add' two amounts of @b@ together.
--
--   * the smallest non-zero currency unit for @b@ ('smallestAmount'). For example, for USD the minimum currency amount
--     is $0.01.
--
--   * how to 'add' two @b@s. By default, this is the 'Monoid' instance's append for @b@.
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
  (UnsafeAmount dis) <> (UnsafeAmount dis') = UnsafeAmount $ dis + dis'

instance (GoodScale (CurrencyScale b), KnownSymbol b) => Monoid (Amount b) where
  mempty = UnsafeAmount $ discrete 0

instance (GoodScale (CurrencyScale b), KnownSymbol b) => IsBet (Amount b) where
  smallestAmount = UnsafeAmount $ discrete 1 :: Amount b
  UnsafeAmount l `minus` UnsafeAmount r
    | r > l = Nothing
    | otherwise = Just $ UnsafeAmount $ l - r
  UnsafeAmount l `add` UnsafeAmount r = UnsafeAmount $ l + r

-- | 'BigBlind' is the type describing poker chip amounts that are measured in big blinds.
--
-- The internal representation of 'BigBlind' is @'Amount' "BB"@. This module introduces
-- a new instance of 'CurrencyScale' (from the
-- <https://hackage.haskell.org/package/safe-money safe-money> package), which allows
-- translation from BigBlind to any valid currency in a lossless manner.
--
-- The small unit of a \"BB\" is a \"bb\", with 100 \"bb\"s in a \"BB\".
--
-- TODO include an API for translating from BigBlind to any safe-money currency, given
-- a 'Poker.Game.Stake'.
--
-- Calculations in the safe-money package are done with Discrete and Dense
-- types. Discrete values are used to describe a regular BigBlind value,
-- such as 1.30bb. Dense values are used when calculating some complex
-- (non-discrete) value such as one third of a big blind. When using the BigBlind
-- type, it is best to do all calculation with Dense "BB" values and then
-- convert back to a Discrete "BB" "bb" after all calculation has been completed:
newtype BigBlind = BigBlind {unBigBlind :: Amount "BB"}
  deriving (Show, Generic, Eq, Ord, IsBet, Semigroup, Monoid)

type instance
  UnitScale "BB" "bb" =
    '(100, 1)

type instance CurrencyScale "BB" = UnitScale "BB" "bb"

-- | When working with a 'BigBlind' you might want to (cautiously) retain losslessness
-- when using functions such as % calculations or division. A 'Dense' allows you to do so.
bigBlindToDense :: BigBlind -> Dense "BB"
bigBlindToDense = denseFromDiscrete . unAmount . unBigBlind
