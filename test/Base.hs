{-# LANGUAGE BlockArguments #-}

module Base where

import Poker.Base
import Instances ()
import Test.Tasty
import Test.Tasty.Hspec
import Test.Tasty.QuickCheck
import Control.Monad
import Control.Applicative
import Control.Exception

spec_CardReadShow :: SpecWith ()
spec_CardReadShow = do
  describe "Check Read instance of Card is Correct" $ forM_ cardCases genTestCase
    where
      genTestCase (cardStr, card) = do
        it ("read " <> cardStr) $ (read cardStr) `shouldBe` card
        it ("show " <> cardStr) $ (show card) `shouldBe` cardStr

prop_badCardRead :: Gen Property
prop_badCardRead = do
    cardStr <- liftA2 (\a b -> a:[b])
                      arbitraryASCIIChar
                      arbitraryASCIIChar
    pure $ not (cardStr `elem` allValidCardStrs) ==>
             ioProperty (isLeft <$> catchPureExceptions (read @Card cardStr))
  where
    catchPureExceptions :: a -> IO (Either SomeException a)
    catchPureExceptions = try @SomeException . evaluate
    allValidCardStrs = fst <$> cardCases
    isLeft = \case
      Left  _ -> True
      Right _ -> False

test_isoReadShow :: TestTree
test_isoReadShow = testProperties "testing isomorphic read . show" $
      [ ( "iso Holding"
        , property $ \hold -> isValidHolding hold ==> testIsoReadShow hold)
      , ( "iso Shape"
        , property $ testIsoReadShow @Shape)
      , ( "iso Rank"
        , property $ testIsoReadShow @Rank)
      , ( "iso Suit"
        , property $ testIsoReadShow @Suit)
      , ( "iso ShapedHand"
        , property $ \sh -> isValidShapedHand sh ==> testIsoReadShow sh)
      ]
  where
    isValidShapedHand :: ShapedHand -> Bool
    isValidShapedHand (ShapedHand (r1, r2) shape) = if r1 == r2
                                    then shape == Pair
                                    else shape == Offsuit || shape == Suited
    isValidHolding :: Holding -> Bool
    isValidHolding (Holdem c1 c2) = c1 /= c2

prop_isoReadShowCard :: Card -> Bool
prop_isoReadShowCard card = read (show card) == card

prop_isoReadShowHolding :: Holding -> Property
prop_isoReadShowHolding holding@(Holdem c1 c2) = c1 /= c2 ==>
                                            read (show holding) == holding

testIsoReadShow :: (Read a, Show a, Eq a) => a -> Bool
testIsoReadShow input = read (show input) == input

cardCases :: [(String, Card)]
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

-- testReadCardFails :: IO ()
-- testReadCardFails = do
--   failStrs <- take 22 . lines <$> readFile "/Users/santi/haskell_exploit/Exploit/test/BaseCards.csv"
--   hspec $ mapM_ checkFailReadCard failStrs
--   where
--     checkFailReadCard cardStr = it ("read " <> cardStr <> " fails") $
--       evaluate (read cardStr :: Card) `shouldThrow` anyErrorCall
--       -- evaluate (read cardStr :: Card) `shouldThrow` errorCall ("Expected Rank string; instead found: " <> cardStr)
--     -- FIXME make this more robust
