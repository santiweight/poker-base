{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}

-- BIG IMPORTANT TODO GET RID OF EQ INSTANCE FOR INDEX

module Poker.Base
  ( module Poker.Types
  , isTableAction, isDealerAction, isPlayerAction
  , inRange, inIndex
  , atPosition
  , sortPreflop, sortPostflop
  , listCard, listRank, listSuit, listShape, listPosition, listHoldemHoldings
  , allShapedHands
  , newDeck
  ) where

import Control.Monad (guard)
import Data.List (sort)
import Poker.Types

isTableAction :: Action -> Bool
isTableAction act = case act of
  MkPlayerAction _ -> True
  _                -> False

isPlayerAction :: Action -> Bool
isPlayerAction act = case act of
  MkPlayerAction _ -> True
  _                -> False

isDealerAction :: Action -> Bool
isDealerAction act = case act of
  MkDealerAction _ -> True
  _ -> False

inRange :: IxRange -> Double -> Bool
inRange (BetweenRn low up) bet = low < bet && bet < up
inRange (AboveRn low) bet = low < bet
inRange (BelowRn up) bet = bet < up
inRange AnyRn _ = True

inIndex :: ActionIx -> BetAction -> Bool
inIndex AnyIx _ = True
inIndex CheckIx Check = True
inIndex (RaiseIx range) (Raise from to) = inRange range (to - from)
inIndex (RaiseOrAllInIx range) (Raise from to) = inRange range (to - from)
inIndex (RaiseOrAllInIx range) (AllIn bet) = inRange range bet
inIndex (AllInIx range) (AllIn allIn) = inRange range allIn
inIndex (BetIx range) (Bet bet) = inRange range bet
inIndex CallIx (Call _) = True
inIndex FoldIx Fold = True
inIndex _ _ = False

atPosition :: Position -> PlayerAction -> Bool
atPosition pos = (pos ==) . position

-- Sort a list of positions according to preflop ordering
sortPreflop :: [Position] -> [Position]
sortPreflop = fmap toEnum . sort . fmap fromEnum

-- Sort a list of positions acccording to postflop ordering
sortPostflop :: [Position] -> [Position]
sortPostflop =
  fmap (toEnum . fromPostFlopOrder)
    . sort
    . fmap (toPostFlopOrder . fromEnum)
  where
    fromPostFlopOrder = flip mod 6 . (+ 4)
    toPostFlopOrder = flip mod 6 . (+ 2)

listCard :: [Card]
listCard = reverse (take 52 (enumFrom (toEnum 0)))

listRank :: [Rank]
listRank = listEnum

listSuit :: [Suit]
listSuit = listEnum

listPosition :: [Position]
listPosition = listEnum :: [Position]

listShape :: [Shape]
listShape = listEnum :: [Shape]

listHoldemHoldings :: [Holding]
listHoldemHoldings = reverse $ do
  rank1 <- listRank
  rank2 <- drop (fromEnum rank1) listRank
  suit1 <- listEnum
  suit2 <-
    if rank1 == rank2
      then drop (fromEnum suit1) listSuit
      else listSuit
  guard (not $ rank1 == rank2 && suit1 == suit2)
  return $ newCombo [Card rank1 suit1, Card rank2 suit2]

allShapedHands :: [ShapedHand]
allShapedHands = reverse $ do
    rank1 <- listRank
    rank2 <- listRank
    return $ newShapedHand (rank1, rank2) $ case compare rank1 rank2 of
        GT -> Suited
        EQ -> Pair
        LT -> Offsuit

listEnum :: (Enum a) => [a]
listEnum = enumFrom (toEnum 0)

newDeck :: Deck
newDeck = Deck $ [Card val su | val <- [Two .. Ace], su <- [Club .. Spade]]

newCombo :: [Card] -> Holding
newCombo [card1, card2] | card1 == card2 = error $ "Cards in combo must be different" ++ show card1 ++ show card2
                          | otherwise      = if card1 > card2
                                               then Holdem card1 card2
                                               else Holdem card2 card1
newCombo _ = error "incorrect number of cards passed to newCombo"

newShapedHand :: (Rank, Rank) -> Shape -> ShapedHand
newShapedHand (rank1, rank2) shape
    | rank1 == rank2 && shape /= Pair = error $ "Pair must be shaped pair, not " ++ show shape
    | rank1 /= rank2 && shape == Pair = error "Non-pair cannot have shape Pair"
    | otherwise      = if rank1 > rank2
                            then ShapedHand (rank1, rank2) shape
                            else ShapedHand (rank2, rank1) shape

-- -- this kinda feels like a mess but might be totally correct?
-- findPositionHolding :: Hand -> Position -> Holding
-- findPositionHolding hand pos =
--   case M.lookup pos (_handSeatMap hand) of
--     Nothing -> error "player given card but no seat"
--     Just posSeat -> case M.lookup posSeat (_handPlayerMap hand) of
--       Nothing -> error "player given position but no such player exists"
--       Just player -> case _playerHolding player of
--         Nothing -> error "player given position but no holding"
--         Just holding -> holding

-- prettyShow :: Hand -> String
-- prettyShow hand = unlines
--     [ show . _handNetwork $ hand
--     , show . _handStakes $ hand
--     , indent <$> unlines $ show <$> _handActions hand
--     ]

