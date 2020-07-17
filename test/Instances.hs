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

instance Arbitrary Action where
  arbitrary = genericArbitrary
  shrink = genericShrink

instance Arbitrary BetAction where
  arbitrary = genericArbitrary
  shrink = genericShrink

instance Arbitrary TableActionValue where
  arbitrary = genericArbitrary
  shrink = genericShrink

instance Arbitrary PlayerAction where
  arbitrary = genericArbitrary
  shrink = genericShrink

instance Arbitrary DealerAction where
  arbitrary = genericArbitrary
  shrink = genericShrink

instance Arbitrary TableAction where
  arbitrary = genericArbitrary
  shrink = genericShrink

instance Arbitrary ActionIx where
  arbitrary = genericArbitrary
  shrink = genericShrink

instance Arbitrary IxRange where
  arbitrary = genericArbitrary
  shrink = genericShrink
