{-# LANGUAGE CPP #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Card types and operators.
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
    mkHole,
    allHoles,
    ShapedHole (..),
    mkPair,
    mkOffsuit,
    mkSuited,
    allShapedHoles,
    holeToShapedHole,
    Deck,
    freshDeck,
    unsafeDeck,
    shapedHoleToHoles,
    rankToChr,
    chrToRank,
    suitToChr,
    chrToSuit,
    cardToShortTxt,
    cardFromShortTxt,
    shapedHoleToShortTxt,
    holeToShortTxt,
    unsafeOffsuit,
    unsafeSuited,
    unsafeHole,
    holeFromShortTxt,
  )
where

#if MIN_VERSION_prettyprinter(1,7,0)
import Prettyprinter
import Prettyprinter.Internal ( unsafeTextWithoutNewlines, Doc(Char) )
#else
import Data.Text.Prettyprint.Doc.Internal
#endif
import Control.Applicative
import Control.Monad
import Data.Bifunctor (Bifunctor (second))
import Data.Maybe
import Data.String (IsString (fromString))
import Data.Text (Text)
import qualified Data.Text as T
import GHC.Generics (Generic)
import Poker.Utils
import Test.QuickCheck (Arbitrary (arbitrary), elements)
import Test.QuickCheck.Arbitrary.Generic (GenericArbitrary (..))

-- $setup
-- >>> import Test.QuickCheck

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
  deriving (Enum, Bounded, Eq, Ord, Show, Read, Generic)
  deriving (Arbitrary) via GenericArbitrary Rank

-- | >>> pretty <$> allRanks
-- [2,3,4,5,6,7,8,9,T,J,Q,K,A]
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
-- >>> chrToRank 'x'
-- Nothing
--
-- prop> \r -> chrToRank (rankToChr r) == Just r
-- +++ OK, passed 100 tests.
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
  deriving (Enum, Bounded, Eq, Ord, Show, Read, Generic)
  deriving (Arbitrary) via GenericArbitrary Suit

-- | >>> pretty allSuits
-- [c, d, h, s]
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
-- >>> chrToSuit 'x'
-- Nothing
--
-- prop> \s -> chrToSuit (suitToChr s) == Just s
-- +++ OK, passed 100 tests.
chrToSuit :: Char -> Maybe Suit
chrToSuit = \case
  'c' -> pure Club
  'd' -> pure Diamond
  'h' -> pure Heart
  's' -> pure Spade
  _ -> Nothing

-- | >>> suitToUnicode <$> [Club, Diamond, Heart, Spade]
-- "\9827\9830\9829\9824"
-- >>> suitFromUnicode . suitToUnicode <$> [Club, Diamond, Heart, Spade]
-- [Just Club,Just Diamond,Just Heart,Just Spade]
suitToUnicode :: Suit -> Char
suitToUnicode = \case
  Club -> '♣'
  Diamond -> '♦'
  Heart -> '♥'
  Spade -> '♠'

-- | >>> suitFromUnicode <$> ['♣', '♦', '♥', '♠']
-- [Just Club,Just Diamond,Just Heart,Just Spade]
-- prop> \s -> suitFromUnicode (suitToUnicode s) == Just s
-- +++ OK, passed 100 tests.
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
  deriving (Eq, Ord, Show, Read, Generic)
  deriving (Arbitrary) via GenericArbitrary Card

-- | >>> pretty ("Ac" :: Card)
-- Ac
instance Pretty Card where
  pretty c = unsafeTextWithoutNewlines $ cardToShortTxt c

instance IsString Card where
  fromString = fromJust . cardFromShortTxt . T.pack

-- | All cards in a 'Deck'
-- >>> length allCards
-- 52
allCards :: [Card]
allCards = liftA2 Card allRanks allSuits

-- | >>> cardToShortTxt "Ac"
-- "Ac"
cardToShortTxt :: Card -> Text
cardToShortTxt (Card r s) = T.pack [rankToChr r, suitToChr s]

-- | >>> cardFromShortTxt "Ac"
-- Just (Card {rank = Ace, suit = Club})
-- prop> \c -> cardFromShortTxt (cardToShortTxt c) == Just c
-- +++ OK, passed 100 tests.
cardFromShortTxt :: Text -> Maybe Card
cardFromShortTxt cs = case second T.uncons <$> T.uncons cs of
  Just (r, Just (s, T.null -> True)) -> Card <$> chrToRank r <*> chrToSuit s
  _ -> Nothing

-- | 'Hole' represents a player's hole cards in a game of Texas Hold\'Em
data Hole = UnsafeHole !Card !Card -- ^ First 'Card' is expected to be '>' the second
  deriving (Eq, Ord, Show, Read, Generic)

-- | Unsafely create a new 'Hole'. The first 'Card' should be '>' than the second.
-- See 'mkHole' for a safe way to create a 'Hole'.
unsafeHole :: Card -> Card -> Hole
unsafeHole = UnsafeHole

-- | >>> "AcKd" :: Hole
-- UnsafeHole (Card {rank = Ace, suit = Club}) (Card {rank = King, suit = Diamond})
instance IsString Hole where
  fromString str =
    fromMaybe invalidHole . holeFromShortTxt $ T.pack str
    where
      invalidHole = error $ "Invalid Hole: " <> str

-- | >>> pretty <$> mkHole (Card Ace Heart) (Card King Spade)
-- Just AhKs
instance Pretty Hole where
  pretty (UnsafeHole c1 c2) = pretty c1 <> pretty c2

-- | The 'Arbitrary' instance for 'Hole' generates values whose 'Card' members
-- are already normalised.
instance Arbitrary Hole where
  arbitrary = elements allHoles

-- | >>> holeToShortTxt "AcKd"
-- "AcKd"
holeToShortTxt :: Hole -> Text
holeToShortTxt (UnsafeHole c1 c2) = cardToShortTxt c1 <> cardToShortTxt c2

-- | >>> holeFromShortTxt "AcKd"
-- Just (UnsafeHole (Card {rank = Ace, suit = Club}) (Card {rank = King, suit = Diamond}))
-- >>> ("KdAc" :: Hole) == "AcKd"
-- True
-- prop> \h -> holeFromShortTxt (holeToShortTxt h) == Just h
-- +++ OK, passed 100 tests.
holeFromShortTxt :: Text -> Maybe Hole
holeFromShortTxt (T.splitAt 2 -> (c1, T.splitAt 2 -> (c2, T.unpack -> []))) =
  join $ mkHole <$> cardFromShortTxt c1 <*> cardFromShortTxt c2
holeFromShortTxt _ = Nothing

-- | Returns a 'Hole' if the incoming 'Card's are unique, else 'Nothing'.
-- Note that 'mkCard' automatically normalises the order of the given 'Card'. See 'Hole' for details.
--
-- prop> \c1 c2 -> mkHole c1 c2 == mkHole c2 c1
-- +++ OK, passed 100 tests.
-- prop> \c1 c2 -> (c1 /= c2) ==> isJust (mkHole c1 c2)
-- +++ OK, passed 100 tests; 2 discarded.
mkHole :: Card -> Card -> Maybe Hole
mkHole c1 c2 =
  if c1 /= c2
    then Just $ if c1 > c2 then UnsafeHole c1 c2 else UnsafeHole c2 c1
    else Nothing

-- | All possible valid 'Hole's (the 'Hole's are also normalised).
-- >>> length allCards * length allCards
-- 2704
-- >>> length allHoles
-- 1326
-- >>> Data.List.nub allHoles == allHoles
-- True
-- >>> pretty $ take 10 allHoles
-- [AsAh, AsAd, AhAd, AsAc, AhAc, AdAc, AsKs, AhKs, AdKs, AcKs]
allHoles :: [Hole]
allHoles = reverse $ do
  r1 <- allRanks
  r2 <- enumFrom r1
  (s1, s2) <-
    if r1 == r2
      then [(s1, s2) | s1 <- allSuits, s2 <- drop 1 (enumFrom s1)]
      else liftA2 (,) allSuits allSuits
  pure $ unsafeHole (Card r2 s2) (Card r1 s1)

-- | A 'ShapedHole' is the 'Suit'-normalised representation of a
-- poker 'Hole'. For example, the 'Hole' "King of Diamonds, 5 of Hearts" is often referred
-- to as "King-5 offsuit".
--
-- To construct a 'ShapedHole', see 'mkPair', 'mkOffsuit', and mkSuited'.
--
-- >>> "22p" :: ShapedHole
-- Pair Two
-- >>> "A4o" :: ShapedHole
-- UnsafeOffsuit Ace Four
-- >>> "KJs" :: ShapedHole
-- UnsafeSuited King Jack
data ShapedHole
  = Pair !Rank
  | UnsafeOffsuit !Rank !Rank -- ^ First 'Rank' should be '>' the second
  | UnsafeSuited !Rank !Rank -- ^ First 'Rank' should be '>' the second
  deriving (Eq, Ord, Show, Read, Generic)

-- | First 'Rank' should '>' than second 'Rank'
unsafeOffsuit :: Rank -> Rank -> ShapedHole
unsafeOffsuit = UnsafeOffsuit

-- | First 'Rank' should be '>' than second 'Rank'
unsafeSuited :: Rank -> Rank -> ShapedHole
unsafeSuited = UnsafeSuited

-- | >>> "AKs" :: ShapedHole
-- UnsafeSuited Ace King
-- >>> "AKo" :: ShapedHole
-- UnsafeOffsuit Ace King
-- >>> "AAp" :: ShapedHole
-- Pair Ace
-- >>> "KAs" == ("AKs" :: ShapedHole)
-- True
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

-- | >>> pretty $ take 10 allShapedHoles
-- [AAp, AKs, AQs, AJs, ATs, A9s, A8s, A7s, A6s, A5s]
instance Pretty ShapedHole where
  pretty = pretty . shapedHoleToShortTxt

-- | The 'Arbitrary' instance for 'ShapedHole' generates values whose 'Rank' members
-- are already normalised.
instance Arbitrary ShapedHole where
  arbitrary = elements allShapedHoles

-- | >>> shapedHoleToShortTxt (mkPair Ace)
-- "AAp"
-- >>> shapedHoleToShortTxt <$> (mkOffsuit Ace King)
-- Just "AKo"
-- >>> shapedHoleToShortTxt <$> (mkSuited Ace King)
-- Just "AKs"
shapedHoleToShortTxt :: ShapedHole -> Text
shapedHoleToShortTxt (UnsafeOffsuit r1 r2) = rankToChr r1 `T.cons` rankToChr r2 `T.cons` "o"
shapedHoleToShortTxt (UnsafeSuited r1 r2) = rankToChr r1 `T.cons` rankToChr r2 `T.cons` "s"
shapedHoleToShortTxt (Pair r) = rankToChr r `T.cons` rankToChr r `T.cons` "p"

-- | Build a pair 'ShapedHole' from the given 'Rank'
mkPair :: Rank -> ShapedHole
mkPair = Pair

-- | Returns a suited 'ShapedHole' if the incoming 'Rank's are unique, else 'Nothing'.
-- Note that 'mkSuited' normalises the order of the incoming 'Rank's.
--
-- prop> \r1 r2 -> mkSuited r1 r2 == mkSuited r2 r1
-- +++ OK, passed 100 tests.
mkSuited :: Rank -> Rank -> Maybe ShapedHole
mkSuited r1 r2 =
  if r1 /= r2
    then Just $ if r1 > r2 then UnsafeSuited r1 r2 else UnsafeSuited r2 r1
    else Nothing

-- | Returns an offsuit 'ShapedHole' if the incoming 'Rank's are unique, else 'Nothing'.
-- Note that the internal representation of 'ShapedHole' is normalised:
--
-- prop> \r1 r2 -> mkOffsuit r1 r2 == mkOffsuit r2 r1
-- +++ OK, passed 100 tests.
mkOffsuit :: Rank -> Rank -> Maybe ShapedHole
mkOffsuit r1 r2 =
  if r1 /= r2
    then Just $ if r1 > r2 then UnsafeOffsuit r1 r2 else UnsafeOffsuit r2 r1
    else Nothing

-- | >>> length allShapedHoles
-- 169
-- >>> Data.List.nub allShapedHoles == allShapedHoles
-- True
-- >>> pretty $ take 15 allShapedHoles
-- [AAp, AKs, AQs, AJs, ATs, A9s, A8s, A7s, A6s, A5s, A4s, A3s, A2s, AKo, KKp]
allShapedHoles :: [ShapedHole]
allShapedHoles = reverse $ do
  rank1 <- allRanks
  rank2 <- allRanks
  return $ case compare rank1 rank2 of
    GT -> unsafeSuited rank1 rank2
    EQ -> mkPair rank1
    LT -> unsafeOffsuit rank2 rank1

-- | >>> fmap holeToShortTxt . shapedHoleToHoles $ "55p"
-- ["5d5c","5h5c","5s5c","5h5d","5s5d","5s5h"]
-- >>> fmap holeToShortTxt . shapedHoleToHoles $ "97o"
-- ["9c7d","9c7h","9c7s","9d7c","9d7h","9d7s","9h7c","9h7d","9h7s","9s7c","9s7d","9s7h"]
-- >>> fmap holeToShortTxt . shapedHoleToHoles $ "QTs"
-- ["QcTc","QdTd","QhTh","QsTs"]
shapedHoleToHoles :: ShapedHole -> [Hole]
shapedHoleToHoles = \case
  Pair r -> do
    s1 <- allSuits
    s2 <- drop (fromEnum s1 + 1) allSuits
    pure . fromJust $ mkHole (Card r s1) (Card r s2)
  UnsafeOffsuit r1 r2 -> do
    s1 <- allSuits
    s2 <- filter (s1 /=) allSuits
    pure . fromJust $ mkHole (Card r1 s1) (Card r2 s2)
  UnsafeSuited r1 r2 -> do
    s <- allSuits
    pure . fromJust $ mkHole (Card r1 s) (Card r2 s)

-- | >>> holeToShapedHole "AcKd"
-- UnsafeOffsuit Ace King
-- >>> holeToShapedHole "AcKc"
-- UnsafeSuited Ace King
-- >>> holeToShapedHole "AcAs"
-- Pair Ace
holeToShapedHole :: Hole -> ShapedHole
holeToShapedHole (UnsafeHole (Card r1 s1) (Card r2 s2))
  | r1 == r2 = mkPair r1
  | s1 == s2 = unsafeSuited r1 r2
  | otherwise = unsafeOffsuit r1 r2

-- | A 'Deck' of 'Card's
newtype Deck = UnsafeDeck [Card] deriving (Read, Show, Eq)

-- | A unshuffled 'Deck' with all 'Card's
-- >>> freshDeck == unsafeDeck allCards
-- True
freshDeck :: Deck
freshDeck = UnsafeDeck allCards

-- | The input 'Card's are not checked in any way.
unsafeDeck :: [Card] -> Deck
unsafeDeck = UnsafeDeck
