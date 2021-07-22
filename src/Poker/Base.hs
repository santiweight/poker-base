{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}

-- BIG IMPORTANT TODO GET RID OF EQ INSTANCE FOR INDEX

{-# LANGUAGE AllowAmbiguousTypes #-}
module Poker.Base
  ( module Poker.Types
  , inIndex
  , atPosition
  , sortPreflop, sortPostflop
  , listCard, listRank, listSuit, listPosition, listHoldemHoldings
  , allShapedHands
  , freshDeck
  ) where

import Control.Monad (guard)
import Data.List (sort)
import Poker.Types
import Money (Dense, Discrete, Discrete')
import Data.Maybe

atPosition :: Position -> PlayerAction t -> Bool
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

listEnum :: (Enum a) => [a]
listEnum = enumFrom (toEnum 0)

listCard :: [Card]
listCard = reverse (take 52 (enumFrom (toEnum 0)))

listRank :: [Rank]
listRank = listEnum

listSuit :: [Suit]
listSuit = listEnum

listPosition :: [Position]
listPosition = listEnum :: [Position]

listHoldemHoldings :: [Hand]
listHoldemHoldings = reverse $ do
  rank1 <- listRank
  rank2 <- drop (fromEnum rank1) listRank
  suit1 <- listEnum
  suit2 <-
    if rank1 == rank2
      then drop (fromEnum suit1) listSuit
      else listSuit
  guard (not $ rank1 == rank2 && suit1 == suit2)
  pure . fromJust $ mkHand (Card rank1 suit1) (Card rank2 suit2)

allShapedHands :: [ShapedHand]
allShapedHands = reverse $ do
    rank1 <- listRank
    rank2 <- listRank
    return $ case compare rank1 rank2 of
        GT -> fromJust $ mkSuited rank1 rank2
        EQ -> mkPair rank1
        LT -> fromJust $ mkOffsuit rank1 rank2

inIndex :: forall b. (Ord b, Num b) => ActionIx b -> BetAction b -> Bool
inIndex = go
  where
    within' = within @b
    go AnyIx _ = True
    go CheckIx Check = True
    go (RaiseIx range) (Raise from to) = within' (to - from) range
    go (RaiseOrAllInIx range) (Raise from to) = within' (to - from) range
    go (RaiseOrAllInIx range) (AllIn bet) = within' bet range
    go (AllInIx range) (AllIn allIn) = within' allIn range
    go (BetIx range) (Bet bet) = within' bet range
    go CallIx (Call _) = True
    go FoldIx Fold = True
    go _ _ = False

-- inRangeAny :: IsBetSize a => IxRange a -> a -> Bool
-- inRangeAny (BetweenRn low up) bet = low `below` bet && bet `below` up
-- inRangeAny (ExactlyRn amount) bet = bet == amount
-- inRangeAny (AboveRn low) bet = low `leq` bet
-- inRangeAny (BelowRn up) bet = bet `leq` up
-- inRangeAny AnyRn _ = True
