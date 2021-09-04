module Test.Poker.Types.Game where

import           Control.Monad.IO.Class         ( liftIO )
import           Data.List.Extra                ( enumerate )
import           Debug.Trace
import           Poker.Types.Game
import           Test.Hspec
import           Test.QuickCheck                ( Gen
                                                , forAll
                                                , shuffle
                                                , sublistOf
                                                )

prop_sortPreflop :: Gen Bool
prop_sortPreflop = do
  somePositions     <- sublistOf $ enumerate @Position
  shuffledPositions <- shuffle somePositions
  pure $ sortPreflop shuffledPositions == somePositions

-- prop_sortPostflop :: Gen Bool
prop_sortPostflop = do
  forAll (sublistOf postFlopPositions) $ \positions ->
    forAll (shuffle positions) $ \shuffledPositions -> do
      sortPostflop shuffledPositions == positions
  where
    postFlopPositions = [SB, BB, UTG, UTG1, UTG2, UTG3, UTG4, UTG5, BU]

spec_sortPositions :: SpecWith ()
spec_sortPositions = do
  describe "sortPreflop" $ do
    preflopCase "empty list"    []     []
    preflopCase "all positions" allPos allPos
    preflopCase "shuffled positions"
                [BU, UTG, BB, UTG1, SB, UTG2]
                [UTG, UTG1, UTG2, BU, SB, BB]
    preflopCase "shuffled positions sublist" [BB, UTG, BU] [UTG, BU, BB]
  describe "sortPostflop" $ do
    postflopCase "empty list"    []     []
    postflopCase "all positions" allPos allPosPostflop
    postflopCase "shuffled positions"
                 [BU, UTG, BB, UTG1, SB, UTG2, UTG4, UTG3, UTG5]
                 allPosPostflop
    postflopCase "shuffled positions sublist" [BB, UTG, BU] [BB, UTG, BU]
 where
  allPos         = enumerate @Position
  allPosPostflop = [SB, BB, UTG, UTG1, UTG2, UTG3, UTG4, UTG5, BU]
  preflopCase    = mkCase sortPreflop
  postflopCase   = mkCase sortPostflop
  mkCase sorter name input expected =
    it name $ sorter input `shouldBe` expected
