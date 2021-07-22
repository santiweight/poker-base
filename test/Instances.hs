{-# OPTIONS_GHC -fno-warn-orphans #-}
module Instances where

import Test.Tasty.QuickCheck
import Test.QuickCheck.Arbitrary.Generic
import Poker.Base
import qualified Data.Range.Range
import GHC.Generics (Generic)

instance Arbitrary Rank where
  arbitrary = genericArbitrary
  shrink = genericShrink

instance Arbitrary Suit where
  arbitrary = genericArbitrary
  shrink = genericShrink

instance Arbitrary Card where
  arbitrary = genericArbitrary
  shrink = genericShrink

instance Arbitrary Hand where
  arbitrary = genericArbitrary
  shrink = genericShrink

instance Arbitrary ShapedHand where
  arbitrary = genericArbitrary
  shrink = genericShrink

instance Arbitrary IsHero where
  arbitrary = genericArbitrary
  shrink = genericShrink

instance Arbitrary Position where
  arbitrary = genericArbitrary
  shrink = genericShrink

instance Arbitrary t => Arbitrary (Action t) where
  arbitrary = genericArbitrary
  shrink = genericShrink

instance Arbitrary t => Arbitrary (BetAction t) where
  arbitrary = genericArbitrary
  shrink = genericShrink

instance Arbitrary t => Arbitrary (TableActionValue t) where
  arbitrary = genericArbitrary
  shrink = genericShrink

instance Arbitrary t => Arbitrary (PlayerAction t) where
  arbitrary = genericArbitrary
  shrink = genericShrink

instance Arbitrary DealerAction where
  arbitrary = genericArbitrary
  shrink = genericShrink

instance Arbitrary t => Arbitrary (TableAction t) where
  arbitrary = genericArbitrary
  shrink = genericShrink

instance Arbitrary a => Arbitrary (ActionIx a) where
  arbitrary = genericArbitrary
  shrink = genericShrink

instance Arbitrary a => Arbitrary (IxRange a) where
  arbitrary = genericArbitrary
  shrink = genericShrink
