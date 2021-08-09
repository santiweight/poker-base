{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLists #-}

module Poker.Types.Cards
  ( Rank(..)
  , Suit(..)
  , Card(..)
  , Hand
  , pattern Hand
  , ParsePretty(..)
  , mkHand
  , enumerate
  , ShapedHand
  , mkPair
  , mkOffsuit
  , mkSuited
  , Deck
  , freshDeck
  , unsafeMkDeck
  , handToShaped
  , shapedHandToHands
  , toUnicode
  , fromUnicode
  , pattern Deck
  , pattern Offsuit
  , pattern Pair
  , pattern Suited
  , unsafeMkHand
  ) where

import           Control.Applicative            ( Alternative(empty)
                                                , Applicative(liftA2)
                                                )
import           Control.Monad                  ( guard
                                                , liftM2
                                                )
import           Data.Data
import           Data.Functor                   ( ($>) )
import qualified Data.IntMap.Strict
import           Data.List.Extra                ( enumerate )
import qualified Data.Map.Strict
import           Data.Maybe                     ( fromJust, fromMaybe )
import           Data.Text.Prettyprint.Doc      ( Pretty(pretty) )
import           GHC.Generics
import           Poker.ParsePretty              ( ParsePretty(parsePrettyP) )
import           Test.QuickCheck.Arbitrary.Generic
import           Text.Megaparsec                ( (<?>)
                                                , MonadParsec(label)
                                                , anySingle
                                                )

-- | The 'Rank' of a playing card
data Rank = Two | Three | Four
          | Five | Six | Seven | Eight | Nine | Ten
          | Jack | Queen | King | Ace
    deriving (Enum, Bounded, Eq, Ord, Data, Typeable, Generic, Show, Read)

instance Pretty Rank where
  pretty = \case
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

instance ParsePretty Rank where
  parsePrettyP = anySingle >>= chrToRank <?> "Rank"
   where
    chrToRank = \case
      '2' -> pure Two
      '3' -> pure Three
      '4' -> pure Four
      '5' -> pure Five
      '6' -> pure Six
      '7' -> pure Seven
      '8' -> pure Eight
      '9' -> pure Nine
      'T' -> pure Ten
      'J' -> pure Jack
      'Q' -> pure Queen
      'K' -> pure King
      'A' -> pure Ace
      chr -> fail $ "Unknown Rank: " <> show chr

-- | The 'Suit' of a playing card
data Suit = Club | Diamond | Heart | Spade
  deriving (Enum, Bounded, Eq, Ord, Data, Typeable, Generic, Show, Read)

-- >>> toUnicode <$> ([Club, Diamond, Heart, Spade] :: [Suit])
-- "\9827\9830\9829\9824"
toUnicode :: Suit -> Char
toUnicode = \case
  Club    -> '♣'
  Diamond -> '♦'
  Heart   -> '♥'
  Spade   -> '♠'

-- >>> toUnicode <$> ([Club, Diamond, Heart, Spade] :: [Suit])
-- "\9827\9830\9829\9824"
fromUnicode :: Char -> Maybe Suit
fromUnicode = \case
  '♣' -> Just Club
  '♦' -> Just Diamond
  '♥' -> Just Heart
  '♠' -> Just Spade
  _   -> Nothing

instance Pretty Suit where
  pretty Club    = "c"
  pretty Diamond = "d"
  pretty Heart   = "h"
  pretty Spade   = "s"

instance ParsePretty Suit where
  parsePrettyP = anySingle >>= chrToSuit <?> "Suit"
   where
    chrToSuit chr = maybe (fail $ "Unexpected Suit: " <> show chr) pure .
      Data.Map.Strict.lookup chr
        . Data.Map.Strict.fromList
        $ [('c', Club), ('d', Diamond), ('h', Heart), ('s', Spade)]

-- | Representation of a playing card.
data Card = Card
  { rank :: Rank
  , suit :: Suit
  }
  deriving (Eq, Ord, Data, Typeable, Generic, Show, Read)

-- | A 'Hand' is the hand that a player was dealt. Currently
-- the 'Hand' type is only for holdem poker
data Hand = MkHand Card Card
  deriving (Eq, Ord, Generic, Show)

{-# COMPLETE Hand #-}
pattern Hand :: Card -> Card -> Hand
pattern Hand c1 c2 <- MkHand c1 c2

mkHand :: Card -> Card -> Maybe Hand
mkHand c1 c2 | c1 /= c2  = Just $ if c2 > c1 then MkHand c2 c1 else MkHand c1 c2
             | otherwise = Nothing

unsafeMkHand :: Card -> Card -> Hand
unsafeMkHand c1 c2 = fromJust $ mkHand c1 c2

allHands :: [Hand]
allHands = reverse $ do
  r1       <- enumerate
  r2       <- enumFrom r1
  (s1, s2) <- if r1 == r2
    then [ (s1, s2) | s1 <- enumerate, s2 <- drop 1 (enumFrom s1) ]
    else liftM2 (,) enumerate enumerate
  pure . fromJust $ mkHand (Card r1 s1) (Card r2 s2)

instance Enum Hand where
  toEnum = fromJust . flip
    Data.IntMap.Strict.lookup
    (Data.IntMap.Strict.fromList $ zip [1 ..] allHands)
  fromEnum = fromJust . flip Data.Map.Strict.lookup
                             (Data.Map.Strict.fromList $ zip allHands [1 ..])

instance Bounded Hand where
  minBound = head allHands
  maxBound = last allHands

-- >>> pretty $ Hand (Card Ace Heart) (Card King Spade)
-- AhKs
instance Pretty Hand where
  pretty (Hand c1 c2) = pretty c1 <> pretty c2

-- >>> parsePretty @Hand "AhKs"
-- Just (Hand (Card {rank = Ace, suit = Heart}) (Card {rank = King, suit = Spade}))
instance ParsePretty Hand where
  parsePrettyP = label "Hand" $ do
    c1 <- parsePrettyP
    c2 <- parsePrettyP
    maybe (fail $ "Invalid card: " <> show (c1, c2)) pure $ mkHand c1 c2

-- | A 'ShapedHand' represents the canonical representation of a
-- poker hand. For example, (King Diamonds, Five Heart), would
-- TODO Make patterns uni-directional (don't expose constructors)
data ShapedHand = MkPair Rank | MkOffsuit Rank Rank | MkSuited Rank Rank
  deriving (Eq, Ord, Generic, Show, Read)

{-# COMPLETE Pair, Offsuit, Suited #-}
pattern Pair :: Rank -> ShapedHand
pattern Pair r1 <- MkPair r1

pattern Offsuit :: Rank -> Rank -> ShapedHand
pattern Offsuit r1 r2 <- MkOffsuit r1 r2

pattern Suited :: Rank -> Rank -> ShapedHand
pattern Suited r1 r2 <- MkSuited r1 r2

mkPair :: Rank -> ShapedHand
mkPair = MkPair

mkSuited :: Rank -> Rank -> Maybe ShapedHand
mkSuited r1 r2
  | r1 /= r2  = Just $ if r1 > r2 then MkSuited r1 r2 else MkSuited r2 r1
  | otherwise = Nothing

mkOffsuit :: Rank -> Rank -> Maybe ShapedHand
mkOffsuit r1 r2
  | r1 /= r2  = Just $ if r1 > r2 then MkOffsuit r1 r2 else MkOffsuit r2 r1
  | otherwise = Nothing

listShapedHands :: [ShapedHand]
listShapedHands = reverse $ do
  rank1 <- enumerate
  rank2 <- enumerate
  return $ case compare rank1 rank2 of
    GT -> fromJust $ mkSuited rank1 rank2
    EQ -> mkPair rank1
    LT -> fromJust $ mkOffsuit rank1 rank2

shapedHandToHands :: ShapedHand -> [Hand]
shapedHandToHands = \case
  Pair r -> do
    s1 <- enumerate
    s2 <- drop (fromEnum s1 + 1) enumerate
    pure . fromJust $ mkHand (Card r s1) (Card r s2)
  Offsuit r1 r2 -> do
    s1 <- enumerate
    s2 <- filter (s1 /=) enumerate
    pure . fromJust $ mkHand (Card r1 s1) (Card r2 s2)
  Suited r1 r2 -> do
    s <- enumerate
    pure . fromJust $ mkHand (Card r1 s) (Card r2 s)

handToShaped :: Hand -> ShapedHand
handToShaped (Hand c1 c2)
  | rank c1 == rank c2 = mkPair $ rank c1
  | suit c1 == suit c2 = fromJust $ mkSuited (rank c1) (rank c2)
  | otherwise          = fromJust $ mkOffsuit (rank c1) (rank c2)

instance Enum ShapedHand where
  toEnum = fromJust . flip
    Data.IntMap.Strict.lookup
    (Data.IntMap.Strict.fromList $ zip [1 ..] listShapedHands)
  fromEnum = fromJust . flip
    Data.Map.Strict.lookup
    (Data.Map.Strict.fromList $ zip listShapedHands [1 ..])

instance Bounded ShapedHand where
  minBound = head listShapedHands
  maxBound = last listShapedHands

-- >>> pretty $ Pair Two
-- 22p
instance Pretty ShapedHand where
  pretty (Offsuit r1 r2) = pretty r1 <> pretty r2 <> "o"
  pretty (Suited  r1 r2) = pretty r1 <> pretty r2 <> "s"
  pretty (Pair r       ) = pretty r <> pretty r <> "p"

-- | pair : 22p
-- offsuit : 24o
-- suited : 24s
-- >>> parsePretty @ShapedHand "22p"
-- Just (Pair Two)
-- >>> parsePretty @ShapedHand "24o"
-- Just (Offsuit Two Four)
-- >>> parsePretty @ShapedHand "24s"
-- Just (Suited Two Four)
instance ParsePretty ShapedHand where
  parsePrettyP = label "ShapedHand" $ do
    r1 <- parsePrettyP @Rank
    r2 <- parsePrettyP @Rank
    anySingle >>= \case
      'p' -> if r1 /= r2
        then fail $ "Pair must have two of same rank, but found " <> show
          (r1, r2)
        else pure $ MkPair r1
      'o' -> if r1 == r2
        then
          fail $ "Offsuit must have two of different rank, but found " <> show
            (r1, r2)
        else pure $ MkPair r1
      's' -> if r1 == r2
        then fail $ "Suited must have two of different rank, but found " <> show
          (r1, r2)
        else pure $ MkPair r1
      _ -> fail "Unexpected hand shape"

newtype Deck = MkDeck [Card] deriving (Read, Show, Eq)

{-# COMPLETE Deck #-}
pattern Deck :: [Card] -> Deck
pattern Deck c1 <- MkDeck c1

-- | A full deck with all cards
-- TODO use template haskell to evaluate at compile time
freshDeck :: Deck
freshDeck = MkDeck allCards

-- | The input cards are not checked in any way.
unsafeMkDeck :: [Card] -> Deck
unsafeMkDeck = MkDeck

instance Pretty Card where
  pretty Card { rank = r, suit = s } = pretty r <> pretty s

instance ParsePretty Card where
  parsePrettyP = liftA2 Card parsePrettyP parsePrettyP

instance Enum Card where
  toEnum n = allCards !! n
  fromEnum c = fromEnum (rank c) * 4 + fromEnum (suit c)

instance Bounded Card where
  minBound = toEnum 0
  maxBound = toEnum 51

-- | All cards in deck
allCards :: [Card]
allCards = liftM2 Card enumerate enumerate