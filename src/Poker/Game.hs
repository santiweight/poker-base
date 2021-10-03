{-# LANGUAGE CPP #-}

-- TODO fix exports
module Poker.Game where

import           Data.List                      ( sort )
import           Poker.Cards
#if MIN_VERSION_prettyprinter(1,7,0)
import Prettyprinter
#else
import Data.Text.Prettyprint.Doc
#endif
import Data.Data
import Poker.Utils (enumerate)

-- | A player's position in a game of poker.
--
-- Future iterations of this library will use a safer/less-hacky representation
-- for 'Position'
data Position = UTG | UTG1 | UTG2 | UTG3 | UTG4 | UTG5 | BU | SB | BB
  deriving (Read, Show, Enum, Bounded, Eq, Ord, Data, Typeable)

instance Pretty Position where
  pretty = viaShow

-- >>> allPositions
-- [UTG,UTG1,UTG2,UTG3,UTG4,UTG5,BU,SB,BB]
allPositions :: [Position]
allPositions = enumerate @Position

-- | Sort a list of positions according to preflop ordering
--
-- WARNING, TODO: This function does not yet handle heads-up appropriately
--
-- >>> sortPreflop $ [BB,BU,UTG1,SB,UTG,UTG2]
-- [UTG,UTG1,UTG2,BU,SB,BB]
-- >>> sortPreflop [SB, BB]
-- [SB,BB]
sortPreflop :: [Position] -> [Position]
sortPreflop = fmap toEnum . sort . fmap fromEnum

-- | Sort a list of positions acccording to postflop ordering
-- >>> sortPostflop $ [BB,BU,UTG1,SB,UTG,UTG2]
-- [SB,BB,UTG,UTG1,UTG2,BU]
-- >>> sortPostflop $ [UTG, SB, BU]
-- [SB,UTG,BU]
-- >>> sortPostflop $ [UTG]
-- [UTG]
sortPostflop :: [Position] -> [Position]
sortPostflop = fmap (toEnum . fromPostFlopOrder) . sort . fmap
  (toPostFlopOrder . fromEnum)
 where
  fromPostFlopOrder = flip mod numPositions . (+ (numPositions - 2))
  toPostFlopOrder   = flip mod numPositions . (+ 2)
  numPositions = fromEnum (maxBound @Position) - fromEnum (minBound @Position) + 1

data IsHero = Hero | Villain
  deriving (Read, Show, Eq, Ord, Enum, Bounded)

newtype Seat = Seat { _unSeat :: Int } deriving (Read, Show, Eq, Ord, Num)

newtype Pot b = Pot { _unPot :: b }
  deriving (Show, Eq, Ord, Num, Functor, Pretty, Semigroup, Monoid)

newtype Stack b = Stack { _unStack :: b }
  deriving (Show, Eq, Ord, Num, Functor, Pretty, Semigroup)

data Board where
  RiverBoard :: !Card -> !Board -> Board
  TurnBoard :: !Card -> !Board -> Board
  FlopBoard :: (Card, Card, Card) -> !Board -> Board
  PreFlopBoard :: !Board -> Board
  InitialTable :: Board
  deriving (Eq, Ord, Show)

instance Pretty Board where
  pretty = viaShow

newtype Stake b = Stake { unStake :: b }
  deriving (Read, Show, Eq, Functor, Ord, Pretty)

data BetAction t
  = Call !t
  | Raise
      { raiseBy :: !t, -- TODO remove?
        raiseTo :: !t
      }
  -- TODO remove AllInRaise
  | AllInRaise
      { amountRaisedAI :: !t, -- TODO remove?
        raisedAITo :: !t
      }
  | Bet !t
  -- TODO remove AllIn
  | AllIn !t
  | Fold
  | Check
  deriving (Read, Show, Eq, Ord, Functor, Data, Typeable)
