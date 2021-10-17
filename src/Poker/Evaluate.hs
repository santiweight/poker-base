{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}

-- | Evaluation of a standard holdem poker hand. 'handRank' works for 5 and 7 card lists.
--
module Poker.Evaluate
  ( -- * ['Card'] Hand Ranking
    HandRank (..),
    handRank,
    allHandRanks,
    straight,
    flush,
    kind,
    rankCount,
    suitRanks,
  )
where

import Control.Applicative
import Data.Bool
import Data.Foldable
import Data.List (sortOn)
import qualified Data.Map.Strict as Map
import Data.Maybe
import Data.Ord
import qualified Data.Set as Set
import Data.Tuple
import GHC.Exts hiding (toList)
import GHC.Generics hiding (from, to)
import Poker.Cards
import Prelude

-- $setup
--
-- >>> import Poker.Cards
-- >>> import Prettyprinter
-- >>> let cs = [Card Ace Heart,Card Seven Spade,Card Ten Heart,Card Five Spade,Card Six Club, Card Seven Heart,Card Six Spade]
-- >>> let cs' = [Card Ten Club, Card Five Spade,Card Ten Heart,Card Five Spade,Card Six Club, Card Seven Heart,Card Six Spade]
-- >>> let css = [cs, cs']
--
-- >>> pretty css
-- [[Ah, 7s, Th, 5s, 6c, 7h, 6s], [Tc, 5s, Th, 5s, 6c, 7h, 6s]]

-- | 5 card standard poker rankings
--
-- >>> handRank <$> css
-- [TwoPair Seven Six Ace,TwoPair Ten Six Five]
data HandRank
  = HighCard Rank Rank Rank Rank Rank
  | OnePair Rank Rank Rank Rank
  | TwoPair Rank Rank Rank
  | ThreeOfAKind Rank Rank Rank
  | Straight Rank
  | Flush Rank Rank Rank Rank Rank
  | FullHouse Rank Rank
  | FourOfAKind Rank Rank
  | StraightFlush Rank
  deriving (Eq, Ord, Show, Generic)

-- | enumeration of all possible HandRanks, in ascending order.
--
-- >>> length allHandRanks
-- 7462
--
allHandRanks :: [HandRank]
allHandRanks =
  [ HighCard a b c d e
    | a <- ranks,
      b <- ranksLT a,
      c <- ranksLT b,
      d <- ranksLT c,
      e <- ranksLT d,
      not (a == succ b && b == succ c && c == succ d && d == s e),
      not (a == Ace && [b, c, d, e] == [Five, Four, Three, Two])
  ]
    ++ [ OnePair a b c d
         | a <- ranks,
           b <- ranks,
           a /= b,
           c <- ranksLT b,
           a /= c,
           d <- ranksLT c,
           a /= d
       ]
    ++ [ TwoPair a b c
         | a <- ranks,
           b <- ranksLT a,
           c <- ranks,
           a /= c,
           b /= c
       ]
    ++ [ ThreeOfAKind a b c
         | a <- ranks,
           b <- ranks,
           a /= b,
           c <- ranksLT b,
           a /= c
       ]
    ++ [Straight f | f <- ranksGE Five]
    ++ [ Flush a b c d e
         | a <- ranks,
           b <- ranksLT a,
           c <- ranksLT b,
           d <- ranksLT c,
           e <- ranksLT d,
           not (a == succ b && b == succ c && c == succ d && d == s e),
           not (a == Ace && [b, c, d, e] == [Five, Four, Three, Two])
       ]
    ++ [FullHouse a b | a <- ranks, b <- ranks, a /= b]
    ++ [FourOfAKind a b | a <- ranks, b <- ranks, a /= b]
    ++ [StraightFlush f | f <- ranksGE Five]
  where
    s Ace = Two
    s other = succ other
    ranks = [Two .. Ace]
    ranksLT Two = []
    ranksLT rank = [Two .. pred rank]
    ranksGE rank = reverse [Ace, King .. rank]

-- | compute a HandRank from a list of Cards.
--
-- Should work for 5 and 7 hand variants.
--
-- >>> handRank cs
-- TwoPair Seven Six Ace
handRank :: [Card] -> HandRank
handRank cs =
  fromMaybe
    (kind (rank <$> cs'))
    ( flush cs'
        <|> straight cs'
    )
  where
    cs' = sortOn Down cs

-- | 5 consecutive card check
--
-- Special rules for an Ace, which can be counted as high or low.
--
-- > run [Ace, King, Queen, Jack, Ten, Nine, Eight]
-- Just Ace
--
-- > run [Ace, King, Queen, Jack, Ten, Eight, Seven]
-- Just Ace
--
-- > run [Ace, King, Queen, Five, Four, Three, Two]
-- Just Five
--
-- > run [Ace, King, Queen, Six, Four, Three, Two]
-- Nothing
run :: [Rank] -> Maybe Rank
run [] = Nothing
run rs@(Ace : rs') = run5 rs <|> bool Nothing (Just Five) (run4 rs' == Just Five)
run rs = run5 rs

-- | Check for a straight. Note holdem rules for an Ace, which can be counted as high or low.
--
-- >>> straight $ (\r -> Card r Heart) <$> [Ace, King, Queen, Five, Four, Three, Two]
-- Just (Straight Five)
--
-- >>> straight cs
-- Nothing
straight :: [Card] -> Maybe HandRank
straight cs = Straight <$> run (Set.toDescList $ ranks cs)
  where
    ranks :: [Card] -> Set.Set Rank
    ranks cs = Set.fromList $ rank <$> cs

run5 :: [Rank] -> Maybe Rank
run5 rs = listToMaybe $ fst <$> filter ((>= 5) . snd) (runs rs)

run4 :: [Rank] -> Maybe Rank
run4 rs = listToMaybe $ fst <$> filter ((>= 4) . snd) (runs rs)

runs :: [Rank] -> [(Rank, Int)]
runs rs = done (foldl' step (Nothing, []) rs)
  where
    step (Nothing, _) r = (Just (r, r), [])
    step (Just (r1, r0), xs) r =
      bool
        -- if gapped then reset, remember old gap
        (Just (r, r), (r0, fromEnum r0 - fromEnum r1 + 1) : xs)
        -- if one less then do nothing
        (Just (r, r0), xs)
        (fromEnum r + 1 == fromEnum r1)
    done (Nothing, xs) = xs
    done (Just (r1, r0), xs) = (r0, fromEnum r0 - fromEnum r1 + 1) : xs

-- | Check if a hand is a Flush or StraightFlush
--
-- >>> flush [Card Ace Heart, Card Seven Club, Card Seven Spade, Card Five Heart, Card Four Heart, Card Three Heart, Card Two Heart]
-- Just (StraightFlush Five)
--
-- >>> flush [Card Ace Heart, Card Seven Club, Card Seven Spade, Card Six Heart, Card Four Heart, Card Three Heart, Card Two Heart]
-- Just (Flush Ace Six Four Three Two)
--
-- >>> flush cs
-- Nothing
flush :: [Card] -> Maybe HandRank
flush cs =
  case filter ((>= 5) . length . snd) (suitRanks cs) of
    [] -> Nothing
    ((_, rs@(r0 : r1 : r2 : r3 : r4 : _)) : _) ->
      Just $
        maybe
          (Flush r0 r1 r2 r3 r4)
          StraightFlush
          (run rs)
    _ -> Nothing

-- | Group Ranks by Suit
--
-- >>> suitRanks cs
-- [(Club,[Six]),(Heart,[Ace,Ten,Seven]),(Spade,[Seven,Five,Six])]
suitRanks :: [Card] -> [(Suit, [Rank])]
suitRanks cs =
  Map.toList $
    Map.fromListWith (flip (<>)) $
      fmap (\(Card r s) -> (s, [r])) cs

-- | count of Ranks from a sorted list
--
-- >>> import Data.Ord
-- >>> import Data.List (sortOn)
-- >>> rankCount (rank <$> (sortOn Down cs))
-- [(Seven,2),(Six,2),(Ace,1),(Ten,1),(Five,1)]
rankCount :: [Rank] -> [(Rank, Int)]
rankCount rs =
  sortOn (Down . swap) $
    Map.toList $
      Map.fromDescListWith (+) $
        fmap (,1) rs

-- | When straights and flushes are ruled out, hand ranking falls back to counted then sorted rank groups, with larger groups (FourOfAKind) ranked higer than smaller ones.
--
-- >>> kind [Ace, Ace, Ace, Ace, Two]
-- FourOfAKind Ace Two
--
-- >>> kind [Ace, Ace, Ace, Two, Two]
-- FullHouse Ace Two
--
-- >>> kind [Ace, Ace, Ace, Five, Two]
-- ThreeOfAKind Ace Five Two
--
-- >>> kind [Ace, Ace, Five, Five, Two]
-- TwoPair Ace Five Two
--
-- >>> kind [Ace, Ace, Six, Five, Two]
-- OnePair Ace Six Five Two
--
-- >>> kind [Ace, King, Six, Five, Two]
-- HighCard Ace King Six Five Two
kind :: [Rank] -> HandRank
kind cs = case rankCount cs of
  ((r0, 4) : (r1, _) : _) -> FourOfAKind r0 r1
  ((r0, 3) : (r1, 3) : _) -> FullHouse r0 r1
  ((r0, 3) : (r1, 2) : _) -> FullHouse r0 r1
  ((r0, 3) : (r1, 1) : (r2, 1) : _) -> ThreeOfAKind r0 r1 r2
  ((r0, 2) : (r1, 2) : (r2, 2) : _) -> TwoPair r0 r1 r2
  ((r0, 2) : (r1, 2) : (r2, 1) : _) -> TwoPair r0 r1 r2
  ((r0, 2) : (r1, 1) : (r2, 1) : (r3, 1) : _) -> OnePair r0 r1 r2 r3
  ((r0, 1) : (r1, 1) : (r2, 1) : (r3, 1) : (r4, 1) : _) -> HighCard r0 r1 r2 r3 r4
  _ -> error ("bad Rank list: " <> show cs)
