{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}

module Poker.Cards
  ( Rank (..),
    allRanks,
    Suit (..),
    allSuits,
    suitToUnicode,
    suitFromUnicode,
    Card (..),
    allCards,
    Hole (..),
    pattern Hole,
    mkHole,
    unsafeMkHole,
    allHoles,
    ShapedHole (..),
    pattern Offsuit,
    pattern Pair,
    pattern Suited,
    mkPair,
    mkOffsuit,
    mkSuited,
    unsafeMkSuited,
    unsafeMkOffsuit,
    allShapedHoles,
    holeToShapedHole,
    Deck (..),
    pattern Deck,
    freshDeck,
    unsafeMkDeck,
    shapedHoleToHoles,
    rankToChr,
    chrToRank,
    suitToChr,
    chrToSuit,
    cardToShortTxt,
    cardFromShortTxt,
  )
where

import Control.Monad (join, liftM2)
#if MIN_VERSION_prettyprinter(1,7,0)
import Prettyprinter
import Prettyprinter.Internal ( unsafeTextWithoutNewlines, Doc(Char) )
#else
import           Data.Text.Prettyprint.Doc.Internal
#endif

import Data.Bifunctor (Bifunctor (second))
import Data.Maybe
import Data.String (IsString (fromString))
import Data.Text (Text)
import qualified Data.Text as T
import GHC.Stack (HasCallStack)
import Poker.Utils

-- | The 'Rank' of a playing 'Card'
data Rank
  = Two
  | Three
  | Four
  | Five
  | Six
  | Seven
  | Eight
  | Nine
  | Ten
  | Jack
  | Queen
  | King
  | Ace
  deriving (Enum, Bounded, Eq, Ord, Show, Read)

instance Pretty Rank where
  pretty = unsafeTextWithoutNewlines . T.singleton . rankToChr

-- | >>> allRanks
-- [Two,Three,Four,Five,Six,Seven,Eight,Nine,Ten,Jack,Queen,King,Ace]
allRanks :: [Rank]
allRanks = enumerate @Rank

-- | >>> rankToChr <$> allRanks
-- "23456789TJQKA"
rankToChr :: Rank -> Char
rankToChr = \case
  Two -> '2'
  Three -> '3'
  Four -> '4'
  Five -> '5'
  Six -> '6'
  Seven -> '7'
  Eight -> '8'
  Nine -> '9'
  Ten -> 'T'
  Jack -> 'J'
  Queen -> 'Q'
  King -> 'K'
  Ace -> 'A'

-- | >>> map (fromJust . chrToRank) "23456789TJQKA"
-- [Two,Three,Four,Five,Six,Seven,Eight,Nine,Ten,Jack,Queen,King,Ace]
-- >>> chrToRank 'f'
-- Nothing
-- prop> chrToRank (rankToChr r) == Just r
chrToRank :: Char -> Maybe Rank
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
  _ -> Nothing

-- | The 'Suit' of a playing 'Card'
data Suit = Club | Diamond | Heart | Spade
  deriving (Enum, Bounded, Eq, Ord, Show, Read)

instance Pretty Suit where
  pretty = Char . suitToChr

-- | >>> allSuits
-- [Club,Diamond,Heart,Spade]
allSuits :: [Suit]
allSuits = enumerate @Suit

-- | >>> suitToChr <$> allSuits
-- "cdhs"
suitToChr :: Suit -> Char
suitToChr = \case
  Club -> 'c'
  Diamond -> 'd'
  Heart -> 'h'
  Spade -> 's'

-- | >>> map (fromJust . chrToSuit) "cdhs"
-- [Club,Diamond,Heart,Spade]
-- >>> chrToSuit '1'
-- Nothing
-- prop> chrToSuit (suitToChr s) == Just s
-- Variable not in scope: s :: Suit
-- Variable not in scope: s :: Suit
chrToSuit :: Char -> Maybe Suit
chrToSuit = \case
  'c' -> pure Club
  'd' -> pure Diamond
  'h' -> pure Heart
  's' -> pure Spade
  _ -> Nothing

-- | >>> suitToUnicode <$> [Club, Diamond, Heart, Spade]
-- "\9827\9830\9829\9824"
-- >>> fromJust . suitFromUnicode . suitToUnicode <$> [Club, Diamond, Heart, Spade]
-- [Club,Diamond,Heart,Spade]
suitToUnicode :: Suit -> Char
suitToUnicode = \case
  Club -> '♣'
  Diamond -> '♦'
  Heart -> '♥'
  Spade -> '♠'

-- >>> suitFromUnicode <$> ['♣', '♦', '♥', '♠']
-- [Just Club,Just Diamond,Just Heart,Just Spade]
suitFromUnicode :: Char -> Maybe Suit
suitFromUnicode = \case
  '♣' -> Just Club
  '♦' -> Just Diamond
  '♥' -> Just Heart
  '♠' -> Just Spade
  _ -> Nothing

-- | Representation of a playing card.
data Card = Card
  { rank :: !Rank,
    suit :: !Suit
  }
  deriving (Eq, Ord, Show, Read)

instance Pretty Card where
  pretty Card {rank = r, suit = s} = pretty r <> pretty s

instance IsString Card where
  fromString = fromJust . cardFromShortTxt . T.pack

-- | All cards in deck
allCards :: [Card]
allCards = liftM2 Card allRanks allSuits

cardToShortTxt :: Card -> Text
cardToShortTxt (Card r s) = T.pack [rankToChr r, suitToChr s]

cardFromShortTxt :: Text -> Maybe Card
cardFromShortTxt cs = case second T.uncons <$> T.uncons cs of
  Just (r, Just (s, T.null -> True)) -> Card <$> chrToRank r <*> chrToSuit s
  _ -> Nothing

-- | 'Hole' represents a player's hole cards in a game of Texas Hold\'Em
data Hole = MkHole !Card !Card
  deriving (Eq, Ord, Show)

instance IsString Hole where
  fromString str = case str of
    [r1, s1, r2, s2] ->
      fromMaybe invalidHole . join $
        mkHole <$> (cardFromShortTxt . T.pack) [r1, s1] <*> (cardFromShortTxt . T.pack) [r2, s2]
    _ -> invalidHole
    where
      invalidHole = error $ "Invalid Hole: " <> str

-- >>> pretty $ Hole (Card Ace Heart) (Card King Spade)
-- AhKs
instance Pretty Hole where
  pretty (Hole c1 c2) = pretty c1 <> pretty c2

{-# COMPLETE Hole #-}

pattern Hole :: Card -> Card -> Hole
pattern Hole c1 c2 <- MkHole c1 c2

-- | Returns a 'Hole' if the incoming 'Card's are unique, else 'Nothing'.
-- Note that the internal representation of 'Hole' is normalised:
--
-- prop> mkHand c1 c2 == mkHand c2 c1
mkHole :: Card -> Card -> Maybe Hole
mkHole c1 c2 =
  if c1 /= c2
    then Just $ if c1 > c2 then MkHole c1 c2 else MkHole c2 c1
    else Nothing

-- | Unsafely creates a new 'Hole'. The two input 'Card's are expected to be
-- unique, and the first 'Card' should be less than the second 'Card' (as defined by
-- 'Ord'). See 'mkHole' for a safe way to create 'Hole's.
unsafeMkHole :: Card -> Card -> Hole
unsafeMkHole c1 c2 =
  fromMaybe (terror $ "Cannot form a Hole from " <> prettyText (c1, c2)) $
    mkHole c1 c2

-- | All possible Hold'Em poker 'Hole's
--
-- TODO add tests
allHoles :: [Hole]
allHoles = reverse $ do
  r1 <- enumerate
  r2 <- enumFrom r1
  (s1, s2) <-
    if r1 == r2
      then [(s1, s2) | s1 <- enumerate, s2 <- drop 1 (enumFrom s1)]
      else liftM2 (,) enumerate enumerate
  pure $ unsafeMkHole (Card r1 s1) (Card r2 s2)

-- |
-- A 'ShapedHole' is the 'Suit'-normalised representation of a
-- poker 'Hole'. For example, the 'Hole' "King of Diamonds, 5 of Hearts" is often referred
-- to as "King-5 offsuit".
--
-- >>> pretty $ mkPair Two
-- 22p
--
-- pair : 22p
-- offsuit : 24o
-- suited : 24s
--
-- >>> "22p" :: ShapedHole
-- MkPair Two
-- >>> "A4o" :: ShapedHole
-- MkOffsuit Ace Four
-- >>> "KJs" :: ShapedHole
-- MkSuited King Jack
--
-- TODO Make patterns uni-directional (don't expose constructors)
data ShapedHole = MkPair !Rank | MkOffsuit !Rank !Rank | MkSuited !Rank !Rank
  deriving (Eq, Ord, Show, Read)

{-# COMPLETE Pair, Offsuit, Suited #-}

pattern Pair :: Rank -> ShapedHole
pattern Pair r <- MkPair r

pattern Offsuit :: Rank -> Rank -> ShapedHole
pattern Offsuit r1 r2 <- MkOffsuit r1 r2

pattern Suited :: Rank -> Rank -> ShapedHole
pattern Suited r1 r2 <- MkSuited r1 r2

instance IsString ShapedHole where
  fromString str = case str of
    [r1, r2, s] ->
      fromMaybe invalidShapedHole $ do
        r1' <- chrToRank r1
        r2' <- chrToRank r2
        case s of
          'p' -> if r1' == r2' then Just $ mkPair r1' else Nothing
          'o' -> mkOffsuit r1' r2'
          's' -> mkSuited r1' r2'
          _ -> Nothing
    _ -> invalidShapedHole
    where
      invalidShapedHole = error $ "Invalid ShapedHole: " <> str

mkPair :: Rank -> ShapedHole
mkPair = MkPair

mkSuited :: Rank -> Rank -> Maybe ShapedHole
mkSuited r1 r2 =
  if r1 /= r2
    then Just $ if r1 > r2 then MkSuited r1 r2 else MkSuited r2 r1
    else Nothing

unsafeMkSuited :: Rank -> Rank -> ShapedHole
unsafeMkSuited r1 r2 =
  fromMaybe (terror $ "Invalid Suited hand: " <> prettyText (r1, r2)) $
    mkSuited r1 r2

mkOffsuit :: Rank -> Rank -> Maybe ShapedHole
mkOffsuit r1 r2 =
  if r1 /= r2
    then Just $ if r1 > r2 then MkOffsuit r1 r2 else MkOffsuit r2 r1
    else Nothing

unsafeMkOffsuit :: HasCallStack => Rank -> Rank -> ShapedHole
unsafeMkOffsuit r1 r2 =
  fromMaybe (terror $ "Cannot form offsuit hand from: " <> prettyText (r1, r2)) $
    mkOffsuit r1 r2

allShapedHoles :: [ShapedHole]
allShapedHoles = reverse $ do
  rank1 <- allRanks
  rank2 <- allRanks
  return $ case compare rank1 rank2 of
    GT -> unsafeMkSuited rank1 rank2
    EQ -> mkPair rank1
    LT -> unsafeMkOffsuit rank1 rank2

-- >>> pretty . shapedHoleToHoles $ "55p"
-- [5d5c, 5h5c, 5s5c, 5h5d, 5s5d, 5s5h]
-- >>> pretty . shapedHoleToHoles $ "97o"
-- [9c7d, 9c7h, 9c7s, 9d7c, 9d7h, 9d7s, 9h7c, 9h7d, 9h7s, 9s7c, 9s7d, 9s7h]
-- >>> pretty . shapedHoleToHoles $ "QTs"
-- [QcTc, QdTd, QhTh, QsTs]
shapedHoleToHoles :: ShapedHole -> [Hole]
shapedHoleToHoles = \case
  Pair r -> do
    s1 <- allSuits
    s2 <- drop (fromEnum s1 + 1) allSuits
    pure . fromJust $ mkHole (Card r s1) (Card r s2)
  Offsuit r1 r2 -> do
    s1 <- allSuits
    s2 <- filter (s1 /=) allSuits
    pure . fromJust $ mkHole (Card r1 s1) (Card r2 s2)
  Suited r1 r2 -> do
    s <- allSuits
    pure . fromJust $ mkHole (Card r1 s) (Card r2 s)

-- TODO needs tests
holeToShapedHole :: Hole -> ShapedHole
holeToShapedHole (Hole (Card r1 s1) (Card r2 s2))
  | r1 == r2 = mkPair r1
  | s1 == s2 = unsafeMkSuited r1 r2
  | otherwise = unsafeMkOffsuit r1 r2

instance Pretty ShapedHole where
  pretty (Offsuit r1 r2) = pretty r1 <> pretty r2 <> "o"
  pretty (Suited r1 r2) = pretty r1 <> pretty r2 <> "s"
  pretty (Pair r) = pretty r <> pretty r <> "p"

newtype Deck = UnsafeMkDeck [Card] deriving (Read, Show, Eq)

{-# COMPLETE Deck #-}

pattern Deck :: [Card] -> Deck
pattern Deck c1 <- UnsafeMkDeck c1

-- | A full deck with all cards
--
-- TODO use template haskell to evaluate at compile time
freshDeck :: Deck
freshDeck = UnsafeMkDeck allCards

-- | The input cards are not checked in any way.
unsafeMkDeck :: [Card] -> Deck
unsafeMkDeck = UnsafeMkDeck
