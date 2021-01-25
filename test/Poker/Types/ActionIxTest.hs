module Poker.Types.ActionIxTest where
import Test.QuickCheck ((==>), (=/=), Arbitrary2(liftArbitrary2), Arbitrary(arbitrary), (===), Gen)
import Test.QuickCheck.Property (withMaxSuccess, Property)
import Control.Applicative (Applicative(liftA2))
import Instances
import Poker.Base (IxRange)
import Debug.Trace (traceShow, traceShowM)
import Poker.Types.ActionIx (exactlyRn, anyRn)
-- import Poker.Types.ActionIx (anyRn)

prop_addRangeAssoc :: IxRange Double -> IxRange Double -> Property
prop_addRangeAssoc l r = withMaxSuccess 100000 $ addRange (+) l r ===  addRange (+) r l
