{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}

module Poker.Cards
  ( Rank(..)
  , Suit(..)
  , Card(..)
  , Hand
  , pattern Hand
  , ParsePretty(..)
  , mkHand
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
  , unsafeMkSuited
  , unsafeMkOffsuit
  ) where

import           Control.Applicative            ( Applicative(liftA2) )
import           Control.Monad                  ( liftM2 )
import qualified Data.IntMap.Strict
import qualified Data.Map.Strict
import           Data.Maybe                     ( fromJust
                                                , fromMaybe
                                                )
import           Data.String                    ( IsString )
import qualified Data.Text                     as T
import           GHC.Exts                       ( IsString(fromString) )
import           Poker.ParsePretty              ( ParsePretty(parsePrettyP)
                                                , parsePretty
                                                , tfailure
                                                )
import           Poker.Utils                    ( atMay
                                                , enumerate
                                                , prettyText
                                                , terror
                                                )
import           Prettyprinter.Internal
import           Text.Megaparsec                ( (<?>)
                                                , MonadParsec(label)
                                                , anySingle
                                                )

-- | The 'Rank' of a playing card
data Rank = Two | Three | Four
          | Five | Six | Seven | Eight | Nine | Ten
          | Jack | Queen | King | Ace
    deriving (Enum, Bounded, Eq, Ord, Show, Read)

instance Pretty Rank where
  pretty r = unsafeTextWithoutNewlines $ case r of
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
      chr -> tfailure $ "Unknown Rank: " <> prettyText chr

-- | The 'Suit' of a playing card
data Suit = Club | Diamond | Heart | Spade
  deriving (Enum, Bounded, Eq, Ord, Show, Read)

-- >>> toUnicode <$> [Club, Diamond, Heart, Spade]
-- "\9827\9830\9829\9824"
-- >>> fromJust . fromUnicode . toUnicode <$> [Club, Diamond, Heart, Spade]
-- [Club,Diamond,Heart,Spade]
toUnicode :: Suit -> Char
toUnicode = \case
  Club    -> '♣'
  Diamond -> '♦'
  Heart   -> '♥'
  Spade   -> '♠'

-- >>> fromUnicode <$> ['♣', '♦', '♥', '♠']
-- [Just Club,Just Diamond,Just Heart,Just Spade]
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
    chrToSuit chr =
      maybe (tfailure $ "Unexpected Suit: " <> prettyText chr) pure
        . Data.Map.Strict.lookup chr
        . Data.Map.Strict.fromList
        $ [('c', Club), ('d', Diamond), ('h', Heart), ('s', Spade)]

-- | Representation of a playing card.
data Card = Card
  { rank :: !Rank
  , suit :: !Suit
  }
  deriving (Eq, Ord, Show, Read)

instance Pretty Card where
  pretty Card { rank = r, suit = s } = pretty r <> pretty s

instance ParsePretty Card where
  parsePrettyP = liftA2 Card parsePrettyP parsePrettyP

instance Enum Card where
  toEnum n =
    fromMaybe (error $ "Invalid Card enum: " <> show n) $ atMay allCards n
  fromEnum c = fromEnum (rank c) * 4 + fromEnum (suit c)

instance Bounded Card where
  minBound = toEnum 0
  maxBound = toEnum 51

-- | All cards in deck
allCards :: [Card]
allCards = liftM2 Card enumerate enumerate

-- | A 'Hand' is the hand that a player was dealt. Currently
-- the 'Hand' type is only for holdem poker
data Hand = MkHand !Card !Card
  deriving (Eq, Ord, Show)

-- TODO tests
instance IsString Hand where
  fromString = fromJust . parsePretty . T.pack

{-# COMPLETE Hand #-}
pattern Hand :: Card -> Card -> Hand
pattern Hand c1 c2 <- MkHand c1 c2

pattern MatchCard :: Int -> Int -> Card
pattern MatchCard r s <- Card (fromEnum -> r) (fromEnum -> s)

mkHand :: Card -> Card -> Maybe Hand
mkHand c1 c2 | c1 /= c2  = Just $ if c2 > c1 then MkHand c2 c1 else MkHand c1 c2
             | otherwise = Nothing

unsafeMkHand :: Card -> Card -> Hand
unsafeMkHand c1 c2 =
  fromMaybe (terror $ "Cannot form a Hand from " <> prettyText (c1, c2))
    $ mkHand c1 c2

allHands :: [Hand]
allHands = reverse $ do
  r1       <- enumerate
  r2       <- enumFrom r1
  (s1, s2) <- if r1 == r2
    then [ (s1, s2) | s1 <- enumerate, s2 <- drop 1 (enumFrom s1) ]
    else liftM2 (,) enumerate enumerate
  pure $ unsafeMkHand (Card r1 s1) (Card r2 s2)

instance Enum Hand where
  toEnum num =
    fromMaybe (error $ "Invalid Hand enum: " <> show num)
      . flip Data.IntMap.Strict.lookup
             (Data.IntMap.Strict.fromList $ zip [1 ..] allHands)
      $ num
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
    maybe (tfailure $ "Invalid card: " <> prettyText (c1, c2)) pure
      $ mkHand c1 c2

-- | A 'ShapedHand' represents the canonical representation of a
-- poker hand. For example, (King Diamonds, Five Heart), would
-- TODO Make patterns uni-directional (don't expose constructors)
data ShapedHand = MkPair !Rank | MkOffsuit !Rank !Rank | MkSuited !Rank !Rank
 deriving (Eq, Ord, Show, Read)

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

unsafeMkSuited :: Rank -> Rank -> ShapedHand
unsafeMkSuited r1 r2 =
  fromMaybe (terror $ "Invalid Suited hand: " <> prettyText (r1, r2))
    $ mkSuited r1 r2

mkOffsuit :: Rank -> Rank -> Maybe ShapedHand
mkOffsuit r1 r2
  | r1 /= r2  = Just $ if r1 > r2 then MkOffsuit r1 r2 else MkOffsuit r2 r1
  | otherwise = Nothing

unsafeMkOffsuit :: Rank -> Rank -> ShapedHand
unsafeMkOffsuit r1 r2 =
  fromMaybe (terror $ "Cannot form offsuit hand from: " <> prettyText (r1, r2))
    $ mkOffsuit r1 r2

listShapedHands :: [ShapedHand]
listShapedHands = reverse $ do
  rank1 <- enumerate
  rank2 <- enumerate
  return $ case compare rank1 rank2 of
    GT -> unsafeMkSuited rank1 rank2
    EQ -> mkPair rank1
    LT -> unsafeMkOffsuit rank1 rank2

-- >>> import Poker.ParsePretty
-- >>> pretty . shapedHandToHands $ unsafeParsePretty "55p"
-- [5d5c, 5h5c, 5s5c, 5h5d, 5s5d, 5s5h]
-- >>> pretty . shapedHandToHands $ unsafeParsePretty "97o"
-- [9c7d, 9c7h, 9c7s, 9d7c, 9d7h, 9d7s, 9h7c, 9h7d, 9h7s, 9s7c, 9s7d, 9s7h]
-- >>> pretty . shapedHandToHands $ unsafeParsePretty "QTs"
-- [QcTc, QdTd, QhTh, QsTs]
shapedHandToHands :: ShapedHand -> [Hand]
shapedHandToHands = \case
  Pair r -> do
    s1 <- enumerate
    s2 <- drop (fromEnum s1 + 1) enumerate
    pure $ unsafeMkHand (Card r s1) (Card r s2)
  Offsuit r1 r2 -> do
    s1 <- enumerate
    s2 <- filter (s1 /=) enumerate
    pure $ unsafeMkHand (Card r1 s1) (Card r2 s2)
  Suited r1 r2 -> do
    s <- enumerate
    pure $ unsafeMkHand (Card r1 s) (Card r2 s)

-- TODO needs tests
handToShaped :: Hand -> ShapedHand
handToShaped (Hand (Card r1 s1) (Card r2 s2))
  | r1 == r2  = mkPair r1
  | s1 == s2  = unsafeMkSuited r1 r2
  | otherwise = unsafeMkOffsuit r1 r1

instance Enum ShapedHand where
  toEnum num =
    fromMaybe (error $ "Invalid ShapedHand enum: " <> show num)
      . flip Data.IntMap.Strict.lookup
             (Data.IntMap.Strict.fromList $ zip [1 ..] listShapedHands)
      $ num
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
-- >>> import Poker.ParsePretty
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
    let rs = (r1, r2)
    anySingle >>= \case
      'p' -> if r1 /= r2 then invalidShapedFail "Pair" rs else pure $ MkPair r1
      'o' -> if r1 == r2
        then invalidShapedFail "Offsuit" rs
        else pure $ unsafeMkOffsuit r1 r2
      's' -> if r1 == r2
        then invalidShapedFail "Suited" rs
        else pure $ unsafeMkSuited r1 r2
      _ -> tfailure "Unexpected hand shape"
   where
    invalidShapedFail name rs =
      tfailure $ "Invalid " <> name <> " ranks: " <> prettyText rs

newtype Deck = UnsafeMkDeck [Card] deriving (Read, Show, Eq)

{-# COMPLETE Deck #-}
{-# LANGUAGE ViewPatterns #-}
pattern Deck :: [Card] -> Deck
pattern Deck c1 <- UnsafeMkDeck c1

-- | A full deck with all cards
-- TODO use template haskell to evaluate at compile time
freshDeck :: Deck
freshDeck = UnsafeMkDeck allCards

-- | The input cards are not checked in any way.
unsafeMkDeck :: [Card] -> Deck
unsafeMkDeck = UnsafeMkDeck
