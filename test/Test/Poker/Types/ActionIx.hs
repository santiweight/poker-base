module Test.Poker.Types.ActionIx where

import Test.QuickCheck ((==>), (=/=), Arbitrary2(liftArbitrary2), Arbitrary(arbitrary), (===), Gen)
import Test.QuickCheck.Property (withMaxSuccess, Property)
import Control.Applicative (Applicative(liftA2))
import Poker.Base (IxRange)
import Debug.Trace (traceShow, traceShowM)
import Poker.Types.ActionIx (exactlyRn, anyRn, addRange)

prop_addRangeAssoc :: IxRange Double -> IxRange Double -> Property
prop_addRangeAssoc l r = addRange l r ===  addRange r l
