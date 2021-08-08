module Test.Poker.Types.Game where

import Test.Tasty.Hspec
import Data.List.Extra ( enumerate )
import Poker.Types.Game

spec_sortPositions :: SpecWith ()
spec_sortPositions = do
  describe "sortPreflop" $ do
    preflopCase "empty list" [] []
    preflopCase "all positions" allPos allPos
    preflopCase "shuffled positions" [BU, UTG, BB, UTG1, SB, UTG2] allPos
    preflopCase "shuffled positions sublist" [BB, UTG, BU] [UTG, BU, BB]
  describe "sortPostflop" $ do
    postflopCase "empty list"    []     []
    postflopCase "all positions" allPos allPosPostflop
    postflopCase "shuffled positions"
                 [BU, UTG, BB, UTG1, SB, UTG2]
                 allPosPostflop
    postflopCase "shuffled positions sublist" [BB, UTG, BU] [BB, UTG, BU]
 where
  allPos         = enumerate @Position
  allPosPostflop = [SB, BB, UTG, UTG1, UTG2, BU]
  preflopCase    = mkCase sortPreflop
  postflopCase   = mkCase sortPostflop
  mkCase sorter name input expected =
    it name $ sorter input `shouldBe` expected
