{-# LANGUAGE CPP #-}
{-# LANGUAGE DuplicateRecordFields #-}

-- | A range of Hole cards.
module Poker.Range
  ( Freq (..),
    Range (..),
    rangeFromList,
    getDecisionFreqRange,
    holdingRangeToShapedRange,
    addHoleToShapedRange,
  )
where

import Data.Bool (bool)
import Data.Map (Map)
import qualified Data.Map.Strict as Map
#if MIN_VERSION_prettyprinter(1,7,0)
import Prettyprinter
#else
import Data.Text.Prettyprint.Doc
#endif
import Poker.Cards

-- $setup
-- >>> :set -XTypeApplications
-- >>> :set -XOverloadedStrings
-- >>> import Poker.Cards
-- >>> import Poker.Range

-- | A frequency is an unevaluated ratio that indicates how often a decision was
-- made. For example, the value Freq (12, 34) indicates that out of the 34
-- people who faced this decision, 12 chose to make this decision.
data Freq = Freq !Int !Int
  deriving (Show, Eq)

instance Monoid Freq where
  mempty = Freq 0 0

instance Semigroup Freq where
  (Freq l1 r1) <> (Freq l2 r2) = Freq (l1 + l2) (r1 + r2)

-- | A simple wrapper around a Map that uses different instances
-- for Semigroup. Normally, the Semigroup instance for Map is a left-biased Map merge.
-- Range merges require Monoid values, and the Note that the internal Map is strict
-- >>> mempty @(Range Hole Freq)
-- Range {_range = fromList []}
-- >>> let left = fromList [("55p" :: ShapedHole, Freq 1 3)]
-- >>> let right = fromList [("55p", Freq 10 32)]
-- >>> left <> right
-- Range {_range = fromList [(Pair Five,Freq 11 35)]}
newtype Range a b = Range
  {_range :: Map a b}
  deriving (Read, Eq, Show)

-- | MAke a Range form a list.
rangeFromList :: Ord a => [(a, b)] -> Range a b
rangeFromList = Range . Map.fromList

instance (Ord a, Monoid b) => Monoid (Range a b) where
  mempty = Range Map.empty

instance (Ord a, Monoid b) => Semigroup (Range a b) where
  Range x <> Range y = Range $ x `uniRange` y
    where
      uniRange = Map.unionWith (<>)

instance (Pretty a, Pretty b) => Pretty (Range a b) where
  pretty ran =
    let m = Map.toList $ _range ran
        prettyValues =
          concatWith (surround comma) $
            map (\(c, i) -> pretty c <+> colon <+> pretty i) m
     in lbrace <+> prettyValues <+> rbrace

-- | Converts a Range from key to action, to a Range from key to decision
-- frequency, given a predicate that returns True if the action matched the
-- decision.
getDecisionFreqRange ::
  Foldable f => (b -> Bool) -> Range a (f b) -> Range a Freq
getDecisionFreqRange p (Range m) =
  Range $ Map.map (foldMap (\v -> Freq (bool 0 1 $ p v) 1)) m

-- | Convert from a Range of hole cards to a Range of shaped hole cards.
holdingRangeToShapedRange :: Monoid v => Range Hole v -> Range ShapedHole v
holdingRangeToShapedRange (Range r) =
  Range $ Map.mapKeysWith (<>) holeToShapedHole r

-- | Add a singleton Hole hand to a Range of shaped hole cards.
addHoleToShapedRange :: Num a => a -> Hole -> Range ShapedHole a -> Range ShapedHole a
addHoleToShapedRange n comb (Range r) =
  Range $ Map.alter (pure . maybe 0 (+ n)) (holeToShapedHole comb) r
