{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE InstanceSigs #-}

module Poker.Types.Cards
  ( Rank(..)
  , Suit(..)
  , Card(..)
  , Hand(..)
  , mkHand
  , ShapedHand(..)
  , mkPair
  , mkOffsuit
  , mkSuited
  , Deck
  , freshDeck
  , unsafeMkDeck
  ) where

import           Control.Applicative
import           Control.Monad                  ( guard )
import           Data.Data
import           Data.Functor                   ( ($>) )
import           GHC.Generics
import           Text.Read


-- | The 'Rank' of a playing card
data Rank = Two | Three | Four
          | Five | Six | Seven | Eight | Nine | Ten
          | Jack | Queen | King | Ace
    deriving (Enum, Eq, Ord, Data, Typeable, Generic)

-- | The 'Suit' of a playing card
--
data Suit = Club | Diamond | Heart | Spade
  deriving (Enum, Eq, Ord, Data, Typeable, Generic)

-- | Representation of a playing card.
data Card = Card
  { rank :: Rank
  , suit :: Suit
  }
  deriving (Eq, Ord, Data, Typeable, Generic)

-- | A 'Hand' is the hand that a player was dealt. Currently
-- the 'Hand' type is only for holdem poker
-- TODO use uni-directional view patterns to expose this functionality
data Hand = Holdem Card Card
  deriving (Eq, Ord, Generic)

mkHand :: Card -> Card -> Maybe Hand
mkHand c1 c2 | c1 /= c2  = Just $ if c1 < c2 then Holdem c1 c2 else Holdem c2 c1
             | otherwise = Nothing

-- | A 'ShapedHand' represents the canonical representation of a
-- poker hand. For example, (King Diamonds, Five Heart), would
-- TODO Make patterns uni-directional (don't expose constructors)
data ShapedHand = Pair Rank | Offsuit Rank Rank | Suited Rank Rank
  deriving (Eq, Ord, Generic)

mkPair :: Rank -> ShapedHand
mkPair = Pair

mkSuited :: Rank -> Rank -> Maybe ShapedHand
mkSuited r1 r2 | r1 /= r2  = Just $ Suited r1 r2
               | otherwise = Nothing

mkOffsuit :: Rank -> Rank -> Maybe ShapedHand
mkOffsuit r1 r2 | r1 /= r2  = Just $ Offsuit r1 r2
                | otherwise = Nothing

newtype Deck = Deck [Card]

freshDeck :: Deck
freshDeck = Deck $ [ Card val su | val <- [Two .. Ace], su <- [Club .. Spade] ]

unsafeMkDeck :: [Card] -> Deck
unsafeMkDeck = Deck

instance Show Suit where
  show Club    = "c"
  show Diamond = "d"
  show Heart   = "h"
  show Spade   = "s"

instance Read Suit where
  readsPrec _ ('c' : cs) = return (Club, cs)
  readsPrec _ ('d' : cs) = return (Diamond, cs)
  readsPrec _ ('h' : cs) = return (Heart, cs)
  readsPrec _ ('s' : cs) = return (Spade, cs)
  readsPrec _ str = error $ "Expected Suit string; instead found: " ++ str

instance Show Rank where
  show = \case
    Two   -> "2"
    Three -> "3"
    Four  -> "4"
    Five  -> "5"
    Six   -> "6"
    Seven -> "7"
    Eight -> "8"
    Nine  -> "9"
    Ten   -> "T"
    Jack  -> "J"
    Queen -> "Q"
    King  -> "K"
    Ace   -> "A"

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

instance Show Hand where
  show (Holdem c1 c2) = show c1 <> show c2

instance Read Hand where
  readPrec = do
    card1 <- readPrec
    card2 <- readPrec
    if card1 == card2 then pfail else pure $ Holdem card1 card2

-- >>> show $ Pair Two
-- "22p"
instance Show ShapedHand where
  show (Offsuit r1 r2) = show r1 <> show r2 <> "o"
  show (Suited  r1 r2) = show r1 <> show r2 <> "s"
  show (Pair r       ) = show r <> show r <> "p"

-- pair : 22p
-- offsuit : 24o
-- suited : 24s
-- >>> read @ShapedHand "22p"
-- 22p
-- >>> read @ShapedHand "24o"
-- 24o
-- >>> read @ShapedHand "24s"
-- 24s
instance Read ShapedHand where
  readPrec :: ReadPrec ShapedHand
  readPrec = do
    r1 <- readPrec @Rank
    r2 <- readPrec @Rank
    get >>= \case
      'p' -> guard (r1 == r2) $> Pair r1
      'o' -> guard (r1 /= r2) $> Offsuit r1 r2
      's' -> guard (r1 /= r2) $> Suited r1 r2
      _   -> pfail


instance Show Card where
  show Card { rank = r, suit = s } = show r ++ show s

instance Read Card where
  readPrec = liftA2 Card readPrec readPrec

instance Enum Card where
  toEnum n | n <= 51   = let (v, s) = n `divMod` 4 in Card (toEnum v) (toEnum s)
           | otherwise = error $ "Not a valid Card enum value: " <> show n
  fromEnum c = fromEnum (rank c) * 4 + fromEnum (suit c)
