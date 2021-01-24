{-# OPTIONS_GHC -fno-warn-orphans #-}
module Instances where

import Test.Tasty.QuickCheck
import Test.QuickCheck.Arbitrary.Generic
import Poker.Base

instance Arbitrary Rank where
  arbitrary = genericArbitrary
  shrink = genericShrink

instance Arbitrary Suit where
  arbitrary = genericArbitrary
  shrink = genericShrink

instance Arbitrary Card where
  arbitrary = genericArbitrary
  shrink = genericShrink

instance Arbitrary Holding where
  arbitrary = genericArbitrary
  shrink = genericShrink

instance Arbitrary Shape where
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

instance Arbitrary t => Arbitrary (ActionIx t) where
  arbitrary = genericArbitrary
  shrink = genericShrink

instance Arbitrary t => Arbitrary (IxRange t) where
  arbitrary = genericArbitrary
  shrink = genericShrink
