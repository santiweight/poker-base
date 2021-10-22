module Test.Poker.Range where

import qualified Data.Map.Strict as Map
import Poker.Range
  ( Freq (Freq),
    Range (Range),
  )
import qualified Poker.Range as Range
import Test.Hspec

spec_RangefromList :: SpecWith ()
spec_RangefromList =
  let m = Range.rangeFromList [(1 :: Int, "bar")]
   in it "fromList" $ m `shouldBe` Range (Map.fromList [(1, "bar")])

spec_RangeMonoid :: SpecWith ()
spec_RangeMonoid = describe "Monoid instance for Range" $ do
  it "mempty is empty map" $
    mempty @(Range Int String)
      `shouldBe` Range Map.empty
  it "<> uses <> itemwise" $
    Range.rangeFromList [(1 :: Int, "foo")]
      <> Range.rangeFromList [(1, "bar")]
      `shouldBe` Range.rangeFromList [(1, "foobar")]
  it "empty items are not lost" $
    Range.rangeFromList [(1 :: Int, "foo")]
      <> Range.rangeFromList [(2, "bar")]
      `shouldBe` Range.rangeFromList [(1, "foo"), (2, "bar")]

spec_FreqMonoid :: SpecWith ()
spec_FreqMonoid = describe "Freq Monoid instance" $ do
  it "mempty is (0, 0)" $ mempty @Freq `shouldBe` Freq 0 0
  it "<> adds each side" $ Freq 0 1 <> Freq 7 11 `shouldBe` Freq 7 12
