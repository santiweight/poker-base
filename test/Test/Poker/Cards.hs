{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Test.Poker.Cards where

import Control.Applicative
import Control.Monad
#if MIN_VERSION_prettyprinter(1,7,0)
import Prettyprinter
#else
import           Data.Text.Prettyprint.Doc
#endif
import Data.Functor
import Data.List.Extra
import Data.Maybe
import Data.String (IsString (fromString))
import Data.Text (Text)
import Poker
import Test.Hspec

spec_rankToChr :: SpecWith ()
spec_rankToChr = do
  it "rankToChr" $ (allRanks <&> rankToChr) `shouldBe` "23456789TJQKA"

spec_chrToRank :: SpecWith ()
spec_chrToRank = do
  it "chrToRank <$> \"23456789TJQKA\"" $ (fromJust . chrToRank <$> "23456789TJQKA") `shouldBe` allRanks
  it "chrToRank '1' == Nothing" $ chrToRank '1' `shouldBe` Nothing

spec_suitToChr :: SpecWith ()
spec_suitToChr = do
  it "suitToChr" $ (allSuits <&> suitToChr) `shouldBe` "cdhs"

spec_chrToSuit :: SpecWith ()
spec_chrToSuit = do
  it "chrToSuit <$> \"cdhs\"" $ (fromJust . chrToSuit <$> "cdhs") `shouldBe` allSuits
  it "chrToSuit '1' == Nothing" $ chrToSuit '1' `shouldBe` Nothing

spec_cardToShortTxt :: SpecWith ()
spec_cardToShortTxt = it "cardToShortTxt" $ forM_ cardCases \(txt, card) -> cardToShortTxt card `shouldBe` txt

spec_cardFromShortTxt :: SpecWith ()
spec_cardFromShortTxt = do
  it "cardFromShortTxt returns Just for all cards" $ forM_ cardCases \(txt, card) -> cardFromShortTxt txt `shouldBe` Just card
  it "cardFromShortTxt \"Ac\" == Just (Card Ace Club)" $ cardFromShortTxt "Ac" `shouldBe` Just (Card Ace Club)
  it "cardFromShortTxt \"Acd\" == Nothing" $ cardFromShortTxt "Acd" `shouldBe` Nothing
  it "cardFromShortTxt \"AcAd\" == Nothing" $ cardFromShortTxt "AcAd" `shouldBe` Nothing

cardCases :: [(Text, Card)]
cardCases =
  [ ("As", Card Ace Spade),
    ("Ah", Card Ace Heart),
    ("Ad", Card Ace Diamond),
    ("Ac", Card Ace Club),
    ("Ks", Card King Spade),
    ("Kh", Card King Heart),
    ("Kd", Card King Diamond),
    ("Kc", Card King Club),
    ("Qs", Card Queen Spade),
    ("Qh", Card Queen Heart),
    ("Qd", Card Queen Diamond),
    ("Qc", Card Queen Club),
    ("Js", Card Jack Spade),
    ("Jh", Card Jack Heart),
    ("Jd", Card Jack Diamond),
    ("Jc", Card Jack Club),
    ("Ts", Card Ten Spade),
    ("Th", Card Ten Heart),
    ("Td", Card Ten Diamond),
    ("Tc", Card Ten Club),
    ("9s", Card Nine Spade),
    ("9h", Card Nine Heart),
    ("9d", Card Nine Diamond),
    ("9c", Card Nine Club),
    ("8s", Card Eight Spade),
    ("8h", Card Eight Heart),
    ("8d", Card Eight Diamond),
    ("8c", Card Eight Club),
    ("7s", Card Seven Spade),
    ("7h", Card Seven Heart),
    ("7d", Card Seven Diamond),
    ("7c", Card Seven Club),
    ("6s", Card Six Spade),
    ("6h", Card Six Heart),
    ("6d", Card Six Diamond),
    ("6c", Card Six Club),
    ("5s", Card Five Spade),
    ("5h", Card Five Heart),
    ("5d", Card Five Diamond),
    ("5c", Card Five Club),
    ("4s", Card Four Spade),
    ("4h", Card Four Heart),
    ("4d", Card Four Diamond),
    ("4c", Card Four Club),
    ("3s", Card Three Spade),
    ("3h", Card Three Heart),
    ("3d", Card Three Diamond),
    ("3c", Card Three Club),
    ("2s", Card Two Spade),
    ("2h", Card Two Heart),
    ("2d", Card Two Diamond),
    ("2c", Card Two Club)
  ]

spec_mkHole :: SpecWith ()
spec_mkHole = do
  let aceS = Card Ace Spade
  let kingD = Card King Diamond
  it "mkHole" $
    mkHole aceS kingD `shouldSatisfy` \case
      Just (UnsafeHole c1 c2) | c1 == aceS, c2 == kingD -> True
      _ -> False
  it "mkHole order doesn't matter" $
    mkHole aceS kingD `shouldBe` mkHole kingD aceS
  it "fail" $ let c = Card Two Club in mkHole c c `shouldBe` Nothing
  it "order doesn't matter" $
    allCardPairs
      `shouldSatisfy` all
        (\(c1, c2) -> mkHole c1 c2 == mkHole c2 c1)
  it "non-equal cards always succeed" $
    allCardPairs
      `shouldSatisfy` all
        (\(c1, c2) -> c1 == c2 || isJust (mkHole c1 c2))
  where
    allCardPairs = [(c1, c2) | c1 <- allCards, c2 <- allCards]

spec_mkShapedHole :: SpecWith ()
spec_mkShapedHole = do
  it "mkPair" $
    mkPair Ace `shouldSatisfy` \case
      Pair Ace -> True
      _ -> False
  it "mkSuited success" $
    mkSuited Ace King
      `shouldSatisfy` ( \case
                          Just (Suited Ace King) -> True
                          _ -> False
                      )
  it "mkSuited wrong order succeeds" $
    mkSuited King Ace
      `shouldSatisfy` ( \case
                          Just (Suited Ace King) -> True
                          _ -> False
                      )
  it "mkSuited failure" $ mkSuited King King `shouldBe` Nothing
  it "mkOffsuit success" $
    mkOffsuit Ace King
      `shouldSatisfy` ( \case
                          Just (Offsuit Ace King) -> True
                          _ -> False
                      )
  it "mkOffsuit wrong order succeeds" $
    mkOffsuit King Ace
      `shouldSatisfy` ( \case
                          Just (Offsuit Ace King) -> True
                          _ -> False
                      )
  it "mkOffsuit failure" $ mkOffsuit King King `shouldBe` Nothing

spec_prettyCard :: SpecWith ()
spec_prettyCard = do
  it "Ac" $ show (pretty (Card Ace Club)) `shouldBe` "Ac"
  it "2h" $ show (pretty (Card Two Heart)) `shouldBe` "2h"

spec_cardIsString :: SpecWith ()
spec_cardIsString = do
  it "Ac is Card Ace Club" $ "Ac" `shouldBe` Card Ace Club
  it "2h is Card Two Heart" $ "2h" `shouldBe` Card Two Heart
  let failCase = failingIsString @Card
  mapM_ failCase ["Ac2h", "AA", "cc"]

spec_holeIsString :: SpecWith ()
spec_holeIsString = do
  it "AcKh" $ "AcKh" `shouldBe` fromJust (mkHole (Card Ace Club) (Card King Heart))
  it "AcKh == KhAc" $ ("AcKh" :: Hole) `shouldBe` "KhAc"
  let failCase = failingIsString @Hole
  mapM_ failCase ["AAKh", "AcKK", "Ac", "AcK", "AcKhQd"]

spec_prettyHole :: SpecWith ()
spec_prettyHole = do
  it "AcKh" $ show (pretty . fromJust $ mkHole (Card Ace Club) (Card King Heart)) `shouldBe` "AcKh"
  it "5s2d" $ show (pretty . fromJust $ mkHole (Card Five Spade) (Card Two Diamond)) `shouldBe` "5s2d"

spec_prettyShapedHole :: SpecWith ()
spec_prettyShapedHole = do
  it "Offsuit" $ show (pretty (mkOffsuit Ace Two)) `shouldBe` "A2o"
  it "Pair" $ show (pretty (mkPair Ace)) `shouldBe` "AAp"
  it "Suited" $ show (pretty (mkSuited Ace Two)) `shouldBe` "A2s"

spec_isStringShapedHole :: SpecWith ()
spec_isStringShapedHole = do
  it "AKo" $ "AKo" `shouldBe` unsafeOffsuit Ace King
  it "AKo == KAo" $ ("AKo" :: ShapedHole) `shouldBe` "KAo"
  it "AKs" $ "AKs" `shouldBe` unsafeSuited Ace King
  it "AKs == KAs" $ ("AKs" :: ShapedHole) `shouldBe` "KAs"
  it "AAp" $ "AAp" `shouldBe` Pair Ace
  let failCase = failingIsString @ShapedHole
  mapM_
    failCase
    ["AKp", "AKf", "AFo", "FKo", "Kp", "AA", "p"]

spec_holeToShaped :: SpecWith ()
spec_holeToShaped = do
  let doCase hole shaped = it (hole <> " => " <> shaped) $ holeToShapedHole (fromString hole) `shouldBe` fromString shaped
  doCase "AcKd" "AKo"
  doCase "AcKc" "AKs"
  doCase "AcAs" "AAp"

spec_shapedHoleToHoles :: SpecWith ()
spec_shapedHoleToHoles = do
  mkCase "Pair" "55p" ["5d5c", "5h5c", "5s5c", "5h5d", "5s5d", "5s5h"]
  mkCase "Suited" "AKs" ["AcKc", "AdKd", "AhKh", "AsKs"]
  mkCase "Offsuit" "QTo" offSuitExpected
  it "Sanity test OffSuit" $ offSuitExpected `shouldSatisfy` not . anySame
  where
    offSuitExpected =
      [ "QcTd",
        "QcTh",
        "QcTs",
        "QdTc",
        "QdTh",
        "QdTs",
        "QhTc",
        "QhTd",
        "QhTs",
        "QsTc",
        "QsTd",
        "QsTh"
      ]
    mkCase name combo expected =
      it name $
        shapedHoleToHoles (fromString combo)
          `shouldBe` fromString
          <$> expected

spec_toUnicode :: SpecWith ()
spec_toUnicode = do
  it "encode Suit" $
    suitToUnicode
      <$> (enumerate @Suit)
      `shouldBe` "\9827\9830\9829\9824" -- TODO fix tasty-discover bug wrt unicode
  it "fromUnicode . toUnicode forms isomorphism" $
    fromJust
      . suitFromUnicode
      . suitToUnicode
      <$> enumerate @Suit
      `shouldBe` enumerate @Suit
  it "fromUnicode fail" $ suitFromUnicode 'b' `shouldBe` Nothing

spec_all :: SpecWith ()
spec_all = do
  describe "enumerate" $ do
    it "Rank" $ allRanks `shouldBe` allRanksExpected
    it "Suit" $ allSuits `shouldBe` allSuitsExpected
    it "Card" $ allCards `shouldBe` allCardsExpected
    it "number of Holes should be 1326" $ length allHoles `shouldBe` 1326
    it "allHoles are unique" $ nub allHoles `shouldBe` allHoles
    it "allHoles Hole cards are valid" $
      allHoles `forM_` flip shouldSatisfy (\(UnsafeHole c1 c2) -> c1 > c2)
    it "number of ShapedHoles should be 169" $
      length allShapedHoles
        `shouldBe` 169
    it "allShapedHoles are unique" $ nub allShapedHoles `shouldBe` allShapedHoles
    it "allShapedHoles" $ allShapedHoles `shouldBe` allShapedHolesExpected
    it "allShapedHoles ShapedHole ranks are valid" $
      allShapedHoles
        `forM_` flip
          shouldSatisfy
          ( \case
              (Pair _) -> True
              (Offsuit r1 r2) -> r1 > r2
              (Suited r1 r2) -> r1 > r2
          )
  where
    allSuitsExpected = [Club, Diamond, Heart, Spade]
    allRanksExpected =
      [ Two,
        Three,
        Four,
        Five,
        Six,
        Seven,
        Eight,
        Nine,
        Ten,
        Jack,
        Queen,
        King,
        Ace
      ]
    allCardsExpected = liftA2 Card allRanks allSuits
    allShapedHolesExpected = ["AAp", "AKs", "AQs", "AJs", "ATs", "A9s", "A8s", "A7s", "A6s", "A5s", "A4s", "A3s", "A2s", "AKo", "KKp", "KQs", "KJs", "KTs", "K9s", "K8s", "K7s", "K6s", "K5s", "K4s", "K3s", "K2s", "AQo", "KQo", "QQp", "QJs", "QTs", "Q9s", "Q8s", "Q7s", "Q6s", "Q5s", "Q4s", "Q3s", "Q2s", "AJo", "KJo", "QJo", "JJp", "JTs", "J9s", "J8s", "J7s", "J6s", "J5s", "J4s", "J3s", "J2s", "ATo", "KTo", "QTo", "JTo", "TTp", "T9s", "T8s", "T7s", "T6s", "T5s", "T4s", "T3s", "T2s", "A9o", "K9o", "Q9o", "J9o", "T9o", "99p", "98s", "97s", "96s", "95s", "94s", "93s", "92s", "A8o", "K8o", "Q8o", "J8o", "T8o", "98o", "88p", "87s", "86s", "85s", "84s", "83s", "82s", "A7o", "K7o", "Q7o", "J7o", "T7o", "97o", "87o", "77p", "76s", "75s", "74s", "73s", "72s", "A6o", "K6o", "Q6o", "J6o", "T6o", "96o", "86o", "76o", "66p", "65s", "64s", "63s", "62s", "A5o", "K5o", "Q5o", "J5o", "T5o", "95o", "85o", "75o", "65o", "55p", "54s", "53s", "52s", "A4o", "K4o", "Q4o", "J4o", "T4o", "94o", "84o", "74o", "64o", "54o", "44p", "43s", "42s", "A3o", "K3o", "Q3o", "J3o", "T3o", "93o", "83o", "73o", "63o", "53o", "43o", "33p", "32s", "A2o", "K2o", "Q2o", "J2o", "T2o", "92o", "82o", "72o", "62o", "52o", "42o", "32o", "22p"]

spec_freshDeck :: SpecWith ()
spec_freshDeck = it "freshDeck" $ freshDeck `shouldBe` unsafeDeck allCards

-- Check that a call to IsString fails for the given type. Values are forced via
-- value's Show instance
failingIsString :: forall a. (IsString a, Show a) => String -> SpecWith ()
failingIsString str = it (str <> " fails") $ print (fromString @a str) `shouldThrow` anyErrorCall
