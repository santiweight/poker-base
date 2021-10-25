{-# LANGUAGE OverloadedStrings #-}

module Test.Poker.Game where

import Poker
import Test.Hspec

spec_allPositions :: SpecWith ()
spec_allPositions = do
  it "heads up all positions" $ allPositions TwoPlayers `shouldBe` [BU, BB]
  it "3max all positions" $ allPositions ThreePlayers `shouldBe` [BU, SB, BB]
  it "4max all positions" $ allPositions FourPlayers `shouldBe` [UTG, BU, SB, BB]
  it "9max all positions" $ allPositions NinePlayers `shouldBe` [UTG, UTG1, UTG2, UTG3, UTG4, UTG5, BU, SB, BB]
  it "10max all positions" $ allPositions TenPlayers `shouldBe` [UTG, UTG1, UTG2, UTG3, UTG4, UTG5, UTG6, BU, SB, BB]
