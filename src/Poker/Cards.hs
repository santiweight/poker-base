{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}

module Poker.Cards
  ( Rank (..),
    allRanks,
    Suit (..),
    allSuits,
    suitToUnicode,
    suitFromUnicode,
    Card (..),
    allCards,
    Hand (..),
    pattern Hand,
    mkHand,
    unsafeMkHand,
    allHands,
    ShapedHand (..),
    pattern Offsuit,
    pattern Pair,
    pattern Suited,
    mkPair,
    mkOffsuit,
    mkSuited,
    unsafeMkSuited,
    unsafeMkOffsuit,
    listShapedHands,
    handToShaped,
    Deck,
    pattern Deck,
    freshDeck,
    unsafeMkDeck,
    shapedHandToHands,
    rankToChr,
    chrToRank,
    suitToChr,
    chrToSuit,
  )
where

import Control.Applicative (Alternative (empty), Applicative (liftA2))
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
import Prettyprinter.Internal ( unsafeTextWithoutNewlines, Doc(Char) )
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
  pretty = unsafeTextWithoutNewlines . T.singleton . rankToChr

instance ParsePretty Rank where
  parsePrettyP = anySingle >>= (maybe empty pure . chrToRank) <?> "Rank"

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

instance ParsePretty Suit where
  parsePrettyP = anySingle >>= (maybe empty pure . chrToSuit) <?> "Suit"

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

-- | 'Hand' represents a player's hole cards in a game of Texas Hold\'Em
data Hand = MkHand !Card !Card
  deriving (Eq, Ord, Show)

-- TODO tests
instance IsString Hand where
  fromString = fromJust . parsePretty . T.pack

{-# COMPLETE Hand #-}

pattern Hand :: Card -> Card -> Hand
pattern Hand c1 c2 <- MkHand c1 c2

-- | Returns a 'Hand' if the incoming 'Card's are unique, else 'Nothing'.
-- Note that the internal representation of 'Hand' is normalised:
--
-- prop> mkHand c1 c2 == mkHand c2 c1
mkHand :: Card -> Card -> Maybe Hand
mkHand c1 c2 =
  if c1 /= c2
    then Just $ if c2 > c1 then MkHand c1 c2 else MkHand c2 c1
    else Nothing

-- | Unsafely creates a new 'Hand'. The two input 'Card's are expected to be
-- unique, and the first 'Card' should be less than the second 'Card' (as defined by
-- 'Ord'). See 'mkHand' for a safe way to create 'Hand's.
unsafeMkHand :: Card -> Card -> Hand
unsafeMkHand c1 c2 =
  fromMaybe (terror $ "Cannot form a Hand from " <> prettyText (c1, c2)) $
    mkHand c1 c2

-- | All possible Hold'Em poker 'Hand's
--
-- TODO add tests
allHands :: [Hand]
allHands = reverse $ do
  r1 <- enumerate
  r2 <- enumFrom r1
  (s1, s2) <-
    if r1 == r2
      then [(s1, s2) | s1 <- enumerate, s2 <- drop 1 (enumFrom s1)]
      else liftM2 (,) enumerate enumerate
  pure $ unsafeMkHand (Card r1 s1) (Card r2 s2)

instance Enum Hand where
  toEnum num =
    fromMaybe (error $ "Invalid Hand enum: " <> show num)
      . flip
        Data.IntMap.Strict.lookup
        (Data.IntMap.Strict.fromList $ zip [1 ..] allHands)
      $ num
  fromEnum =
    fromJust
      . flip
        Data.Map.Strict.lookup
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
    maybe (tfailure $ "Invalid card: " <> prettyText (c1, c2)) pure $
      mkHand c1 c2

-- |
-- A 'ShapedHand' is the 'Suit'-normalised representation of a
-- poker 'Hand'. For example, the 'Hand' "King of Diamonds, 5 of Hearts" is often referred
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
-- >>> parsePretty @ShapedHand "22p"
-- Just (MkPair Two)
-- >>> parsePretty @ShapedHand "24o"
-- Just (MkOffsuit Four Two)
-- >>> parsePretty @ShapedHand "24s"
-- Just (MkSuited Four Two)
--
-- TODO Make patterns uni-directional (don't expose constructors)
data ShapedHand = MkPair !Rank | MkOffsuit !Rank !Rank | MkSuited !Rank !Rank
  deriving (Eq, Ord, Show, Read)

{-# COMPLETE Pair, Offsuit, Suited #-}

pattern Pair :: Rank -> ShapedHand
pattern Pair r <- MkPair r

pattern Offsuit :: Rank -> Rank -> ShapedHand
pattern Offsuit r1 r2 <- MkOffsuit r1 r2

pattern Suited :: Rank -> Rank -> ShapedHand
pattern Suited r1 r2 <- MkSuited r1 r2

mkPair :: Rank -> ShapedHand
mkPair = MkPair

mkSuited :: Rank -> Rank -> Maybe ShapedHand
mkSuited r1 r2
  | r1 /= r2 = Just $ if r1 > r2 then MkSuited r1 r2 else MkSuited r2 r1
  | otherwise = Nothing

unsafeMkSuited :: Rank -> Rank -> ShapedHand
unsafeMkSuited r1 r2 =
  fromMaybe (terror $ "Invalid Suited hand: " <> prettyText (r1, r2)) $
    mkSuited r1 r2

mkOffsuit :: Rank -> Rank -> Maybe ShapedHand
mkOffsuit r1 r2
  | r1 /= r2 = Just $ if r1 > r2 then MkOffsuit r1 r2 else MkOffsuit r2 r1
  | otherwise = Nothing

unsafeMkOffsuit :: HasCallStack => Rank -> Rank -> ShapedHand
unsafeMkOffsuit r1 r2 =
  fromMaybe (terror $ "Cannot form offsuit hand from: " <> prettyText (r1, r2)) $
    mkOffsuit r1 r2

listShapedHands :: [ShapedHand]
listShapedHands = reverse $ do
  rank1 <- enumerate
  rank2 <- enumerate
  return $ case compare rank1 rank2 of
    GT -> unsafeMkSuited rank1 rank2
    EQ -> mkPair rank1
    LT -> unsafeMkOffsuit rank1 rank2

-- | >>> import Poker.ParsePretty
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
  | r1 == r2 = mkPair r1
  | s1 == s2 = unsafeMkSuited r1 r2
  | otherwise = unsafeMkOffsuit r1 r2

instance Enum ShapedHand where
  toEnum num =
    fromMaybe (error $ "Invalid ShapedHand enum: " <> show num)
      . flip
        Data.IntMap.Strict.lookup
        (Data.IntMap.Strict.fromList $ zip [1 ..] listShapedHands)
      $ num
  fromEnum =
    fromJust
      . flip
        Data.Map.Strict.lookup
        (Data.Map.Strict.fromList $ zip listShapedHands [1 ..])

instance Bounded ShapedHand where
  minBound = head listShapedHands
  maxBound = last listShapedHands

instance Pretty ShapedHand where
  pretty (Offsuit r1 r2) = pretty r1 <> pretty r2 <> "o"
  pretty (Suited r1 r2) = pretty r1 <> pretty r2 <> "s"
  pretty (Pair r) = pretty r <> pretty r <> "p"

instance ParsePretty ShapedHand where
  parsePrettyP = label "ShapedHand" $ do
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
