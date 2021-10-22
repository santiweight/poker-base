{-# LANGUAGE OverloadedStrings #-}

module Test.Poker.Game where

import Poker
import Test.Hspec

spec_allPositions :: SpecWith ()
spec_allPositions = do
  it "heads up all positions" $ (positionToTxt TwoPlayers <$> allPositions TwoPlayers) `shouldBe` ["BU", "BB"]
  it "3max all positions" $ (positionToTxt ThreePlayers <$> allPositions ThreePlayers) `shouldBe` ["BU", "SB", "BB"]
  it "4max all positions" $ (positionToTxt FourPlayers <$> allPositions FourPlayers) `shouldBe` ["CO", "BU", "SB", "BB"]
  it "9max all positions" $ (positionToTxt NinePlayers <$> allPositions NinePlayers) `shouldBe` ["UTG", "UTG1", "UTG2", "LJ", "HJ", "CO", "BU", "SB", "BB"]
