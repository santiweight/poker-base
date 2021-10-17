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
    Deck,
    pattern Deck,
    freshDeck,
    unsafeMkDeck,
    shapedHoleToHoles,
  )
where

import Control.Applicative (Applicative (liftA2))
import Control.Monad (liftM2)
import qualified Data.IntMap.Strict
import qualified Data.Map.Strict
import Data.Maybe
  ( fromJust,
    fromMaybe,
  )
import Data.String (IsString)
import qualified Data.Text as T
#if MIN_VERSION_prettyprinter(1,7,0)
import Prettyprinter
import Prettyprinter.Internal ( unsafeTextWithoutNewlines )
#else
import           Data.Text.Prettyprint.Doc.Internal
#endif
import GHC.Exts (IsString (fromString))
import GHC.Stack (HasCallStack)
import Poker.ParsePretty
  ( ParsePretty (parsePrettyP),
    parsePretty,
    tfailure,
  )
import Poker.Utils
  ( atMay,
    enumerate,
    prettyText,
    terror,
  )
import Text.Megaparsec
  ( MonadParsec (label),
    anySingle,
    (<?>),
  )

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
  pretty r = unsafeTextWithoutNewlines $ case r of
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

-- | >>> allRanks
-- [Two,Three,Four,Five,Six,Seven,Eight,Nine,Ten,Jack,Queen,King,Ace]
allRanks :: [Rank]
allRanks = enumerate @Rank

-- | The 'Suit' of a playing 'Card'
data Suit = Club | Diamond | Heart | Spade
  deriving (Enum, Bounded, Eq, Ord, Show, Read)

instance Pretty Suit where
  pretty Club = "c"
  pretty Diamond = "d"
  pretty Heart = "h"
  pretty Spade = "s"

instance ParsePretty Suit where
  parsePrettyP = anySingle >>= chrToSuit <?> "Suit"
    where
      chrToSuit chr =
        maybe (tfailure $ "Unexpected Suit: " <> prettyText chr) pure
          . Data.Map.Strict.lookup chr
          . Data.Map.Strict.fromList
          $ [('c', Club), ('d', Diamond), ('h', Heart), ('s', Spade)]

-- | >>> allSuits
-- [Club,Diamond,Heart,Spade]
allSuits :: [Suit]
allSuits = enumerate @Suit

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
allCards = liftM2 Card allRanks allSuits

-- | 'Hole' represents a player's hole cards in a game of Texas Hold\'Em
data Hole = MkHole !Card !Card
  deriving (Eq, Ord, Show)

-- TODO tests
instance IsString Hole where
  fromString = fromJust . parsePretty . T.pack

{-# COMPLETE Hole #-}

pattern Hole :: Card -> Card -> Hole
pattern Hole c1 c2 <- MkHole c1 c2

-- | Returns a 'Hole' if the incoming 'Card's are unique, else 'Nothing'.
-- Note that the internal representation of 'Hole' is normalised:
--
-- prop> mkHand c1 c2 == mkHand c2 c1
mkHole :: Card -> Card -> Maybe Hole
mkHole c1 c2
  | c1 /= c2 = Just $ if c2 > c1 then MkHole c2 c1 else MkHole c1 c2
  | otherwise = Nothing

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

instance Enum Hole where
  toEnum num =
    fromMaybe (error $ "Invalid Hole enum: " <> show num)
      . flip
        Data.IntMap.Strict.lookup
        (Data.IntMap.Strict.fromList $ zip [1 ..] allHoles)
      $ num
  fromEnum =
    fromJust
      . flip
        Data.Map.Strict.lookup
        (Data.Map.Strict.fromList $ zip allHoles [1 ..])

instance Bounded Hole where
  minBound = head allHoles
  maxBound = last allHoles

-- >>> pretty $ Hole (Card Ace Heart) (Card King Spade)
-- AhKs
instance Pretty Hole where
  pretty (Hole c1 c2) = pretty c1 <> pretty c2

-- >>> parsePretty @Hole "AhKs"
-- Just (Hole (Card {rank = Ace, suit = Heart}) (Card {rank = King, suit = Spade}))
instance ParsePretty Hole where
  parsePrettyP = label "Hole" $ do
    c1 <- parsePrettyP
    c2 <- parsePrettyP
    maybe (tfailure $ "Invalid card: " <> prettyText (c1, c2)) pure $
      mkHole c1 c2

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
-- >>> import Poker.ParsePretty
-- >>> parsePretty @ShapedHole "22p"
-- Just (MkPair Two)
-- >>> parsePretty @ShapedHole "24o"
-- Just (MkOffsuit Four Two)
-- >>> parsePretty @ShapedHole "24s"
-- Just (MkSuited Four Two)
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

mkPair :: Rank -> ShapedHole
mkPair = MkPair

mkSuited :: Rank -> Rank -> Maybe ShapedHole
mkSuited r1 r2
  | r1 /= r2 = Just $ if r1 > r2 then MkSuited r1 r2 else MkSuited r2 r1
  | otherwise = Nothing

unsafeMkSuited :: Rank -> Rank -> ShapedHole
unsafeMkSuited r1 r2 =
  fromMaybe (terror $ "Invalid Suited hand: " <> prettyText (r1, r2)) $
    mkSuited r1 r2

mkOffsuit :: Rank -> Rank -> Maybe ShapedHole
mkOffsuit r1 r2
  | r1 /= r2 = Just $ if r1 > r2 then MkOffsuit r1 r2 else MkOffsuit r2 r1
  | otherwise = Nothing

unsafeMkOffsuit :: HasCallStack => Rank -> Rank -> ShapedHole
unsafeMkOffsuit r1 r2 =
  fromMaybe (terror $ "Cannot form offsuit hand from: " <> prettyText (r1, r2)) $
    mkOffsuit r1 r2

allShapedHoles :: [ShapedHole]
allShapedHoles = reverse $ do
  rank1 <- enumerate
  rank2 <- enumerate
  return $ case compare rank1 rank2 of
    GT -> unsafeMkSuited rank1 rank2
    EQ -> mkPair rank1
    LT -> unsafeMkOffsuit rank1 rank2

-- | >>> import Poker.ParsePretty
-- >>> pretty . shapedHoleToHoles $ unsafeParsePretty "55p"
-- [5d5c, 5h5c, 5s5c, 5h5d, 5s5d, 5s5h]
-- >>> pretty . shapedHoleToHoles $ unsafeParsePretty "97o"
-- [9c7d, 9c7h, 9c7s, 9d7c, 9d7h, 9d7s, 9h7c, 9h7d, 9h7s, 9s7c, 9s7d, 9s7h]
-- >>> pretty . shapedHoleToHoles $ unsafeParsePretty "QTs"
-- [QcTc, QdTd, QhTh, QsTs]
shapedHoleToHoles :: ShapedHole -> [Hole]
shapedHoleToHoles = \case
  Pair r -> do
    s1 <- enumerate
    s2 <- drop (fromEnum s1 + 1) enumerate
    pure $ unsafeMkHole (Card r s1) (Card r s2)
  Offsuit r1 r2 -> do
    s1 <- enumerate
    s2 <- filter (s1 /=) enumerate
    pure $ unsafeMkHole (Card r1 s1) (Card r2 s2)
  Suited r1 r2 -> do
    s <- enumerate
    pure $ unsafeMkHole (Card r1 s) (Card r2 s)

-- TODO needs tests
holeToShapedHole :: Hole -> ShapedHole
holeToShapedHole (Hole (Card r1 s1) (Card r2 s2))
  | r1 == r2 = mkPair r1
  | s1 == s2 = unsafeMkSuited r1 r2
  | otherwise = unsafeMkOffsuit r1 r2

instance Enum ShapedHole where
  toEnum num =
    fromMaybe (error $ "Invalid ShapedHole enum: " <> show num)
      . flip
        Data.IntMap.Strict.lookup
        (Data.IntMap.Strict.fromList $ zip [1 ..] allShapedHoles)
      $ num
  fromEnum =
    fromJust
      . flip
        Data.Map.Strict.lookup
        (Data.Map.Strict.fromList $ zip allShapedHoles [1 ..])

instance Bounded ShapedHole where
  minBound = head allShapedHoles
  maxBound = last allShapedHoles

instance Pretty ShapedHole where
  pretty (Offsuit r1 r2) = pretty r1 <> pretty r2 <> "o"
  pretty (Suited r1 r2) = pretty r1 <> pretty r2 <> "s"
  pretty (Pair r) = pretty r <> pretty r <> "p"

instance ParsePretty ShapedHole where
  parsePrettyP = label "ShapedHole" $ do
    r1 <- parsePrettyP @Rank
    r2 <- parsePrettyP @Rank
    let rs = (r1, r2)
    anySingle >>= \case
      'p' -> if r1 /= r2 then invalidShapedFail "Pair" rs else pure $ MkPair r1
      'o' ->
        if r1 == r2
          then invalidShapedFail "Offsuit" rs
          else pure $ unsafeMkOffsuit r1 r2
      's' ->
        if r1 == r2
          then invalidShapedFail "Suited" rs
          else pure $ unsafeMkSuited r1 r2
      _ -> tfailure "Unexpected hand shape"
    where
      invalidShapedFail name rs =
        tfailure $ "Invalid " <> name <> " ranks: " <> prettyText rs

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
