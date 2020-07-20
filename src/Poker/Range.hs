{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DuplicateRecordFields #-}
-- {-# LANGUAGE FlexibleContexts #-}
-- {-# LANGUAGE MultiParamTypeClasses #-}

module Poker.Range
    ( Range(..), range
    , RangeCollection(..), ranges, total_range, total_shapedrange
    , CountRange
    , FreqRange
    , ShapedRangeC
    , ShapedRange
    , addComboToCollection
    , shapedRangeToFreqRange
    , shapedHoldingRangeToShapedFreqRange
    -- , rangeToFreqRange
    , shapeToCombos
    , findRange
    , holdingRangeToShapedRange
    , combineCountRanges
    , IndexedRangeC
    , addCombo
    ) where

import Control.Lens
import Data.Aeson
import Data.List
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe
import Data.Monoid (Sum (..))
import Poker.Base

newtype Range a b
  = Range
      {_range :: Map a b}
  deriving (Read, Eq, ToJSON, FromJSON)

instance (Ord a, Monoid b) => Semigroup (Range a b) where
  Range x <> Range y = Range $ x `uniRange` y
    where
      uniRange = Map.unionWith (<>)

instance (Ord a, Monoid b) => Monoid (Range a b) where
  mempty = Range Map.empty

data RangeCollection a b
  = RangeCollection
      { _ranges :: Map a b,
        _total_range :: CountRange,
        _total_shapedrange :: ShapedCountRange
      }
  -- , _total_count :: Int }
  deriving (Show, Eq)

type Freq = Double
type Count = Sum Int
type FreqRange = Range Holding Freq
type CountRange = Range Holding Count
type ShapedCountRange = Range ShapedHand Count
type ShapedRange = Range ShapedHand CountRange
type FreqShapedRange = Range ShapedHand FreqRange
type ShapedRangeC = RangeCollection BetAction ShapedRange
type IndexedRangeC = RangeCollection ActionIx FreqShapedRange

instance (Show a, Show b) => Show (Range a b) where
  show ran =
    let rangemap = Map.toList $ _range ran
        rangelist = intercalate "," $ map (\(c, i) -> show c ++ ":" ++ show i) rangemap
     in "[" ++ rangelist ++ "]" -- ++ "|" ++ show (_count ran) ++ "]"

$(makeLenses ''Range)
$(makeLenses ''RangeCollection)

holdingRangeToShapedRange :: Range Holding [BetAction] -> Range ShapedHand (Range Holding [BetAction])
holdingRangeToShapedRange (Range r) = Range $ Map.foldrWithKey combine mempty r
  where
    combine ::
      Holding ->
      [BetAction] ->
      Map ShapedHand (Range Holding [BetAction]) ->
      Map ShapedHand (Range Holding [BetAction])
    combine holding acts seed =
      seed
        & at (comboToShaped holding) . non mempty . range . at holding %~ Just . maybe acts (acts <>)

shapedRangeToFreqRange :: (BetAction -> Bool) -> Range ShapedHand (Range Holding [BetAction])
  -> Range ShapedHand (Range Holding (Int, Int))
shapedRangeToFreqRange prop ran =
  ran & range . each %~ flatten :: Range ShapedHand (Range Holding (Int, Int))
  where
    flatten :: Range Holding [BetAction] -> Range Holding (Int, Int)
    flatten (Range r) = Range $ r & each %~ (\ls -> (length (filter prop ls), length ls))

shapedHoldingRangeToShapedFreqRange :: Range ShapedHand (Range Holding (Int, Int)) -> Range ShapedHand (Int, Int)
shapedHoldingRangeToShapedFreqRange ran = ran & range . each %~ sumRatios
  where
    sumRatios :: Range Holding (Int, Int) -> (Int, Int)
    sumRatios (Range r) = foldr (\(numAcc, denAcc) (numNew, denNew) -> (numAcc + numNew, denAcc + denNew)) (0, 0) r

inc :: Count -> Maybe Count -> Maybe Count
inc n mayNum = return $ fromMaybe 0 mayNum & (n +)

addCombo :: Count -> Holding -> CountRange -> CountRange
addCombo n comb ran =
  ran
    -- & count %~ (+ n) -- here
    & range . at comb %~ inc n -- here

comboToShaped :: Holding -> ShapedHand
comboToShaped (Holdem c1 c2)
  | rank c1 == rank c2 = ShapedHand (rank c1, rank c2) Pair
  | suit c1 == suit c2 = ShapedHand (rank c1, rank c2) Suited
  | otherwise = ShapedHand (rank c1, rank c2) Offsuit

addComboToShaped :: Count
                      -> Holding
                      -> Range ShapedHand CountRange
                      -> Range ShapedHand CountRange
addComboToShaped n comb ran =
    let shaped_hand = comboToShaped comb in
    ran & range . at shaped_hand . non mempty %~ addCombo n comb -- here
        -- & count +~ n -- here

addShaped :: Count
               -> Holding -> Range ShapedHand Count -> Range ShapedHand Count
addShaped n comb ran =
    let shaped_hand = comboToShaped comb in
    ran & range . at shaped_hand %~ inc n
        -- & count +~ n

addComboToCollection :: Ord a =>
                          Count
                          -> a
                          -> Holding
                          -> RangeCollection a (Range ShapedHand CountRange)
                          -> RangeCollection a (Range ShapedHand CountRange)
addComboToCollection n act comb ranC =
  ranC
    & ranges . at act . non mempty %~ addComboToShaped n comb
    & total_range %~ addCombo n comb
    & total_shapedrange %~ addShaped n comb
-- & total_count %~ (n +)

shapeToCombos :: ShapedHand -> [Holding]
shapeToCombos (ShapedHand (r1, r2) shape) =
  case shape of
    Pair -> do
      s1 <- listSuit
      s2 <- drop (fromEnum s1 + 1) listSuit
      return $ Holdem (Card r1 s1) (Card r2 s2)
    Offsuit -> do
      s1 <- listSuit
      s2 <- filter (s1 ==) listSuit
      return $ Holdem (Card r1 s1) (Card r2 s2)
    Suited -> do
      suit_ <- listSuit
      return $ Holdem (Card r1 suit_) (Card r2 suit_)

-- rangeToFreqRange :: ShapedRangeC -> FreqShapedRangeC
-- rangeToFreqRange ranC =
--   let cc = ranC ^. total_range . range
--       new_ranges = Map.map (fn2 cc) $ ranC ^. ranges
--    in RangeCollection new_ranges (ranC ^. total_range) (ranC ^. total_shapedrange)
--   where
--     fn2 cc shaped_range = shaped_range & range .~ Map.map (fn3 cc) (shaped_range ^. range)
--     fn3 cc count_range =
--       count_range
--         { _range =
--             Map.mapWithKey (\c n -> fromIntegral n / fromIntegral (cc ! c)) (count_range ^. range)
--         }

findRange :: ActionIx -> RangeCollection BetAction ShapedRange -> ShapedRange
findRange indx ranC =
  let unionF = unionIndexRange indx
   in Map.foldlWithKey unionF mempty (ranC ^. ranges)

unionIndexRange :: ActionIx -> ShapedRange -> BetAction -> ShapedRange -> ShapedRange
unionIndexRange indx resRange k ran =
  if inIndex indx k
    then
      resRange -- & count +~ ran ^. count
        & range %~ Map.unionWith combineCountRanges (ran ^. range)
    else resRange

combineCountRanges :: CountRange -> CountRange -> CountRange
combineCountRanges countR1 countR2 =
  countR1
    -- & count +~ countR2 ^. count
    & range %~ Map.union (countR2 ^. range)