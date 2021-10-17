{-# LANGUAGE OverloadedStrings #-}

module Test.Poker.Game where

import Poker
import Test.Hspec

spec_allPositions :: SpecWith ()
spec_allPositions = do
  it "heads up all positions" $ (positionToTxt players2 <$> allPositions players2) `shouldBe` ["BU", "BB"]
  it "3max all positions" $ (positionToTxt players3 <$> allPositions players3) `shouldBe` ["BU", "SB", "BB"]
  it "4max all positions" $ (positionToTxt players4 <$> allPositions players4) `shouldBe` ["CO", "BU", "SB", "BB"]
  it "9max all positions" $ (positionToTxt players9 <$> allPositions players9) `shouldBe` ["UTG","UTG1","UTG2","LJ","HJ","CO","BU","SB","BB"]