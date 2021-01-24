{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Poker.Types.Cards where

import GHC.Generics
import Data.Data
import Text.Read
import Control.Applicative

-- | The 'Rank' of a playing card
data Rank = Two | Three | Four
          | Five | Six | Seven | Eight | Nine | Ten
          | Jack | Queen | King | Ace
    deriving (Enum, Eq, Ord, Data, Typeable, Generic)

-- | The 'Suit' of a playing card
data Suit = Club | Diamond | Heart | Spade
  deriving (Enum, Eq, Ord, Data, Typeable, Generic)

-- | Representation of a playing card.
data Card
  = Card
      { rank :: Rank,
        suit :: Suit
      }
  deriving (Eq, Data, Typeable, Generic)

-- | A 'Holding' is the hand that a player was dealt. Currently
-- the 'Holding' type is only for holdem poker
data Holding
  = Holdem Card Card
  deriving (Eq, Ord, Generic)


-- | A 'ShapedHand' represents the canonical representation of a
-- poker hand. For example, (King Diamonds, Five Heart), would
data ShapedHand = ShapedHand (Rank, Rank) Shape
  deriving (Eq, Ord, Generic)

data Shape = Offsuit | Suited | Pair
  deriving (Eq, Ord, Enum, Generic)

newtype Deck = Deck [Card]

instance Show Suit where
  show Club = "c"
  show Diamond = "d"
  show Heart = "h"
  show Spade = "s"

instance Read Suit where
  readsPrec _ ('c' : cs) = return (Club, cs)
  readsPrec _ ('d' : cs) = return (Diamond, cs)
  readsPrec _ ('h' : cs) = return (Heart, cs)
  readsPrec _ ('s' : cs) = return (Spade, cs)
  readsPrec _ str = error $ "Expected Suit string; instead found: " ++ str

instance Show Rank where
  show = \case
    Two -> "2"
    Three -> "3"
    Four -> "4"
    Five -> "5"
    Six -> "6"
    Seven -> "7"
    Eight -> "8"
    Nine -> "9"
    Ten -> "T"
    Jack -> "J"
    Queen -> "Q"
    King -> "K"
    Ace -> "A"

instance Read Rank where
  readPrec = get >>= maybe pfail pure . chrToRank
    where
      chrToRank :: Char -> Maybe Rank
      chrToRank = \case
                    '2' -> Just Two
                    '3' -> Just Three
                    '4' -> Just Four
                    '5' -> Just Five
                    '6' -> Just Six
                    '7' -> Just Seven
                    '8' -> Just Eight
                    '9' -> Just Nine
                    'T' -> Just Ten
                    'J' -> Just Jack
                    'Q' -> Just Queen
                    'K' -> Just King
                    'A' -> Just Ace
                    _   -> Nothing

instance Show Shape where
  show = \case
    Offsuit -> "o"
    Pair -> "p"
    Suited -> "s"

instance Read Shape where
  readPrec = get >>= maybe pfail pure . chrToShape
    where
      chrToShape :: Char -> Maybe Shape
      chrToShape = \case
                    'o' -> Just Offsuit
                    'p' -> Just Pair
                    's' -> Just Suited
                    _   -> Nothing

instance Show Holding where
  show (Holdem c1 c2) = show c1 <> show c2

instance Read Holding where
  readPrec = do
    card1 <- readPrec
    card2 <- readPrec
    if card1 == card2
      then pfail
      else pure $ Holdem card1 card2

instance Show ShapedHand where
  show (ShapedHand (r1, r2) shp) = show r1 ++ show r2 ++ show shp

instance Read ShapedHand where
  readPrec =
    liftA3
      (\c1 c2 s -> ShapedHand (c1, c2) s)
      readPrec
      readPrec
      readPrec

instance Read Card where
  readPrec = liftA2 Card readPrec readPrec

instance Show Card where
  show Card{rank=r, suit=s} = show r ++ show s

instance Ord Card where
  compare c1 c2 = compare (rank c1, suit c1) (rank c2, suit c2)

instance Enum Card where
      toEnum n | n <= 51 = let (v,s) = n`divMod`4 in Card (toEnum v) (toEnum s)
               | otherwise = error ""
      fromEnum c = fromEnum (rank c) * 4 + fromEnum (suit c)
