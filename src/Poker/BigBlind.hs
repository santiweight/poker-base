{-# LANGUAGE UndecidableInstances #-}

-- TODO fix exports
module Poker.BigBlind where

import           GHC.Generics                   ( Generic )
import           Money
import           Poker.Amount


{- |

Calculations in the safe-money package are done with Discrete and Dense
types. Discrete values are used to describe a regular BigBlind value,
such as 1.30bb. Dense values are used when calculating some complex
(non-discrete) value such as one third of a big blind.30bb. When using the BigBlind
type, it is best to do all calculation with Dense "BB" values and then
convert back to a Discrete "BB" "bb" after all calculation has been completed:

TODO add examples
-}
type instance UnitScale "BB" "bb"
  = '(100, 1)

type instance CurrencyScale "BB" = UnitScale "BB" "bb"

{- |
'BigBlind' is encoded by adding a new currency \"BB\" to those currencies
supported in <https://hackage.haskell.org/package/safe-money safe-money>.
The small unit of a \"BB\" is a \"bb\", with 100 \"bb\"s in a \"BB\"
-}
newtype BigBlind = BigBlind { unBigBlind :: Amount "BB" }
  deriving (Show, Generic, Eq, Ord, IsBet, Semigroup, Monoid)

-- | When working with a 'BigBlind' you might want to (cautiously) do non-lossless
-- functions such as % calculations or division. A 'Dense' allows you to do so.
bigBlindToDense :: BigBlind -> Dense "BB"
bigBlindToDense = denseFromDiscrete . unAmount . unBigBlind
