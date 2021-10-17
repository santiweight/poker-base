{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE CPP #-}
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
import Data.Text (Text)
import Poker
import Test.Hspec
import Data.String (IsString(fromString))

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
      Just (Hole c1 c2) | c1 == aceS, c2 == kingD -> True
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
    allCardPairs = [(c1, c2) | c1 <- enumerate, c2 <- enumerate]

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

spec_prettyShapedHole :: SpecWith ()
spec_prettyShapedHole = do
  it "Offsuit" $ show (pretty (mkOffsuit Ace Two)) `shouldBe` "A2o"
  it "Pair" $ show (pretty (mkPair Ace)) `shouldBe` "AAp"
  it "Suited" $ show (pretty (mkSuited Ace Two)) `shouldBe` "A2s"

-- TODO Make sure test is total
-- TODO Make sure all ShapedHoles are generated at the right frequency
spec_handToShaped :: SpecWith ()
spec_handToShaped = pure ()

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
    it "number of Holes should be 1326" $
      length (enumerate @Hole)
        `shouldBe` 1326
    it "number of Holes should be 1326" $
      length (nub $ enumerate @Hole)
        `shouldBe` 1326
    it "number of ShapedHoles should be 169" $
      length (enumerate @ShapedHole)
        `shouldBe` 169
    it "number of unique ShapedHoles should be 169" $
      length (nub $ enumerate @ShapedHole)
        `shouldBe` 169
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

spec_freshDeck :: SpecWith ()
spec_freshDeck = it "freshDeck" $ freshDeck `shouldBe` unsafeMkDeck allCards
