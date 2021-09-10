{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE AllowAmbiguousTypes #-}

module Test.Poker.Cards where

import           Control.Applicative
import           Control.Monad
import           Data.List                      ( nub )
import           Data.List.Extra                ( anySame )
import           Data.Maybe                     ( fromJust
                                                , isJust
                                                , isNothing
                                                )
import           Data.Text                      ( Text )
import qualified Data.Text                     as T
import           Data.Text.Prettyprint.Doc
import           Poker
import           Poker.Internal
import           Test.Hspec
import           Test.Tasty.QuickCheck
import Poker.Internal (enumerate)

spec_CardPrettyAndParse :: SpecWith ()
spec_CardPrettyAndParse = do
  describe "Check Pretty and ParsePretty form isomorphism"
    $ forM_ cardCases genTestCase
 where
  genTestCase (cardStr, card) = do
    it (T.unpack $ "parsePretty " <> cardStr)
      $          parsePretty cardStr
      `shouldBe` Just card

prop_badCardParse :: Gen Property
prop_badCardParse = do
  cardStr <- liftA2 (\a b -> T.pack [a, b])
                    arbitraryASCIIChar
                    arbitraryASCIIChar
  pure $ cardStr `notElem` allValidCardStrs ==> isNothing
    (parsePretty @Card cardStr)
  where allValidCardStrs = fst <$> cardCases

cardCases :: [(Text, Card)]
cardCases =
  [ ("As", Card Ace Spade)
  , ("Ah", Card Ace Heart)
  , ("Ad", Card Ace Diamond)
  , ("Ac", Card Ace Club)
  , ("Ks", Card King Spade)
  , ("Kh", Card King Heart)
  , ("Kd", Card King Diamond)
  , ("Kc", Card King Club)
  , ("Qs", Card Queen Spade)
  , ("Qh", Card Queen Heart)
  , ("Qd", Card Queen Diamond)
  , ("Qc", Card Queen Club)
  , ("Js", Card Jack Spade)
  , ("Jh", Card Jack Heart)
  , ("Jd", Card Jack Diamond)
  , ("Jc", Card Jack Club)
  , ("Ts", Card Ten Spade)
  , ("Th", Card Ten Heart)
  , ("Td", Card Ten Diamond)
  , ("Tc", Card Ten Club)
  , ("9s", Card Nine Spade)
  , ("9h", Card Nine Heart)
  , ("9d", Card Nine Diamond)
  , ("9c", Card Nine Club)
  , ("8s", Card Eight Spade)
  , ("8h", Card Eight Heart)
  , ("8d", Card Eight Diamond)
  , ("8c", Card Eight Club)
  , ("7s", Card Seven Spade)
  , ("7h", Card Seven Heart)
  , ("7d", Card Seven Diamond)
  , ("7c", Card Seven Club)
  , ("6s", Card Six Spade)
  , ("6h", Card Six Heart)
  , ("6d", Card Six Diamond)
  , ("6c", Card Six Club)
  , ("5s", Card Five Spade)
  , ("5h", Card Five Heart)
  , ("5d", Card Five Diamond)
  , ("5c", Card Five Club)
  , ("4s", Card Four Spade)
  , ("4h", Card Four Heart)
  , ("4d", Card Four Diamond)
  , ("4c", Card Four Club)
  , ("3s", Card Three Spade)
  , ("3h", Card Three Heart)
  , ("3d", Card Three Diamond)
  , ("3c", Card Three Club)
  , ("2s", Card Two Spade)
  , ("2h", Card Two Heart)
  , ("2d", Card Two Diamond)
  , ("2c", Card Two Club)
  ]

spec_isoPrettyParseRank :: SpecWith ()
spec_isoPrettyParseRank = checkPrettyParseEnumIso @Rank "Rank"

spec_isoPrettyParseSuit :: SpecWith ()
spec_isoPrettyParseSuit = checkPrettyParseEnumIso @Suit "Suit"

spec_isoPrettyParseCard :: SpecWith ()
spec_isoPrettyParseCard = checkPrettyParseEnumIso @Card "Card"

spec_isoPrettyParseHand :: SpecWith ()
spec_isoPrettyParseHand = checkPrettyParseEnumIso @Hand "Hand"

spec_isoPrettyParseShapedHand :: SpecWith ()
spec_isoPrettyParseShapedHand = checkPrettyParseEnumIso @ShapedHand "ShapeHand"

checkPrettyParseEnumIso
  :: forall a
   . (Eq a, ParsePretty a, Pretty a, Enum a, Bounded a, Show a)
  => String
  -> SpecWith ()
checkPrettyParseEnumIso typeName =
  describe ("pretty and parse form iso" <> typeName)
    $ mapM_ (\val -> it (show val) $ roundTrip val == val) (enumerate @a)
 where
  renderPrettyT = T.pack . show . pretty
  roundTrip     = unsafeParsePretty . renderPrettyT

spec_mkHand :: SpecWith ()
spec_mkHand = do
  let aceS = Card Ace Spade
  let aceD = Card Ace Diamond
  it "success" $ mkHand aceS aceD `shouldSatisfy` \case
    Just (Hand c1 c2) | c1 == aceS, c2 == aceD -> True
    _ -> False
  it "fail" $ let c = Card Two Club in mkHand c c `shouldBe` Nothing
  it "order doesn't matter" $ allCardPairs `shouldSatisfy` all
    (\(c1, c2) -> mkHand c1 c2 == mkHand c2 c1)
  it "non-equal cards always succeed" $ allCardPairs `shouldSatisfy` all
    (\(c1, c2) -> c1 == c2 || isJust (mkHand c1 c2))
  where allCardPairs = [ (c1, c2) | c1 <- enumerate, c2 <- enumerate ]

spec_mkShapedHand :: SpecWith ()
spec_mkShapedHand = do
  it "mkPair" $ mkPair Ace `shouldSatisfy` \case
    Pair Ace -> True
    _        -> False
  it "mkSuited success"
    $               mkSuited Ace King
    `shouldSatisfy` (\case
                      Just (Suited Ace King) -> True
                      _                      -> False
                    )
  it "mkSuited wrong order succeeds"
    $               mkSuited King Ace
    `shouldSatisfy` (\case
                      Just (Suited Ace King) -> True
                      _                      -> False
                    )
  it "mkSuited failure" $ mkSuited King King `shouldBe` Nothing
  it "mkOffsuit success"
    $               mkOffsuit Ace King
    `shouldSatisfy` (\case
                      Just (Offsuit Ace King) -> True
                      _                       -> False
                    )
  it "mkOffsuit wrong order succeeds"
    $               mkOffsuit King Ace
    `shouldSatisfy` (\case
                      Just (Offsuit Ace King) -> True
                      _                       -> False
                    )
  it "mkOffsuit failure" $ mkOffsuit King King `shouldBe` Nothing

spec_prettyShapedHand :: SpecWith ()
spec_prettyShapedHand = do
  it "Offsuit" $ show (pretty (mkOffsuit Ace Two)) `shouldBe` "A2o"
  it "Pair" $ show (pretty (mkPair Ace)) `shouldBe` "AAp"
  it "Suited" $ show (pretty (mkSuited Ace Two)) `shouldBe` "A2s"

-- TODO Make sure test is total
-- TODO Make sure all ShapedHands are generated at the right frequency
spec_handToShaped :: SpecWith ()
spec_handToShaped = pure ()

spec_shapedHandToHands :: SpecWith ()
spec_shapedHandToHands = do
  mkCase "Pair"    "55p" ["5d5c", "5h5c", "5s5c", "5h5d", "5s5d", "5s5h"]
  mkCase "Suited"  "AKs" ["AcKc", "AdKd", "AhKh", "AsKs"]
  mkCase "Offsuit" "QTo" offSuitExpected
  it "Sanity test OffSuit" $ offSuitExpected `shouldSatisfy` not . anySame
 where
  offSuitExpected =
    [ "QcTd"
    , "QcTh"
    , "QcTs"
    , "QdTc"
    , "QdTh"
    , "QdTs"
    , "QhTc"
    , "QhTd"
    , "QhTs"
    , "QsTc"
    , "QsTd"
    , "QsTh"
    ]
  mkCase name combo expected =
    it name
      $          shapedHandToHands (unsafeParsePretty combo)
      `shouldBe` unsafeParsePretty
      <$>        expected

spec_toUnicode :: SpecWith ()
spec_toUnicode = do
  it "encode Suit"
    $          toUnicode
    <$>        (enumerate @Suit)
    `shouldBe` "\9827\9830\9829\9824" -- TODO fix tasty-discover bug wrt unicode
  it "fromUnicode . toUnicode forms isomorphism"
    $          fromJust
    .          fromUnicode
    .          toUnicode
    <$>        enumerate @Suit
    `shouldBe` enumerate @Suit
  it "fromUnicode fail" $ fromUnicode 'b' `shouldBe` Nothing

spec_enumerate :: SpecWith ()
spec_enumerate = do
  describe "enumerate" $ do
    it "Rank" $ enumerate @Rank `shouldBe` allRanks
    it "Suit" $ enumerate @Suit `shouldBe` allSuits
    it "Card" $ enumerate @Card `shouldBe` allCards
    it "number of Hands should be 1326"
      $          length (enumerate @Hand)
      `shouldBe` 1326
    it "number of Hands should be 1326"
      $          length (nub $ enumerate @Hand)
      `shouldBe` 1326
    it "number of ShapedHands should be 169"
      $          length (enumerate @ShapedHand)
      `shouldBe` 169
    it "number of unique ShapedHands should be 169"
      $          length (nub $ enumerate @ShapedHand)
      `shouldBe` 169

spec_freshDeck :: SpecWith ()
spec_freshDeck = it "freshDeck" $ freshDeck `shouldBe` unsafeMkDeck allCards

allSuits :: [Suit]
allSuits = [Club, Diamond, Heart, Spade]

allRanks :: [Rank]
allRanks =
  [Two, Three, Four, Five, Six, Seven, Eight, Nine, Ten, Jack, Queen, King, Ace]

allCards :: [Card]
allCards = liftA2 Card allRanks allSuits
