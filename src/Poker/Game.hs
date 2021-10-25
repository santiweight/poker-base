{-# LANGUAGE OverloadedStrings #-}

-- | Representation of a game of holdem, including table structure, positioning, pot and betting state.
module Poker.Game
  ( Position (..),
    NumPlayers (..),
    numPlayersToWord8,
    numPlayersFromWord8,
    mkNumPlayers,
    allPositions,
    allPossiblePositions,
    getPreflopOrder,
    buttonPosition,
    smallBlindPosition,
    bigBlindPosition,
    getPostFlopOrder,
    sortPostflop,
    sortPreflop,
    Seat (..),
    Pot (..),
    Stack (..),
    Stake (..),
    BetAction (..),
    IsHero (..),
    Board (..),
  )
where

-- ,smallBlindPosition,sortPreflop)

import Data.Data
import Data.Word
import Poker.Cards
import Poker.Utils (enumerate)
import Prettyprinter

-- | A player's 'Position' in a game of poker.
--
-- 'Position's are ordered by table order (clockwise). The smallest 'Position', @Position 0@,
-- is the first player to act preflop. The largest 'Position' is always the big blind.
--
-- >>> allPositions TwoPlayers
-- [BU,BB]
-- >>> allPositions SixPlayers
-- [UTG,UTG1,UTG2,BU,SB,BB]
-- >>> allPositions TenPlayers
-- [UTG,UTG1,UTG2,UTG3,UTG4,UTG5,UTG6,BU,SB,BB]
--
-- The API for 'Position' is unstable. We are open to better ideas :)
data Position = UTG | UTG1 | UTG2 | UTG3 | UTG4 | UTG5 | UTG6 | BU | SB | BB
  deriving (Read, Show, Enum, Bounded, Eq, Ord, Data, Typeable)

instance Pretty Position where
  pretty = viaShow

-- | Number of active players at a poker table. Players sitting out do not count, as
-- they do not contribute to the number of 'Position's.
data NumPlayers
  = TwoPlayers
  | ThreePlayers
  | FourPlayers
  | FivePlayers
  | SixPlayers
  | SevenPlayers
  | EightPlayers
  | NinePlayers
  | TenPlayers
  deriving (Show, Enum, Eq, Ord, Bounded)

-- | Convert a 'NumPlayers' to a 'Word8'.
numPlayersToWord8 :: NumPlayers -> Word8
numPlayersToWord8 TwoPlayers = 2
numPlayersToWord8 ThreePlayers = 3
numPlayersToWord8 FourPlayers = 4
numPlayersToWord8 FivePlayers = 5
numPlayersToWord8 SixPlayers = 6
numPlayersToWord8 SevenPlayers = 7
numPlayersToWord8 EightPlayers = 8
numPlayersToWord8 NinePlayers = 9
numPlayersToWord8 TenPlayers = 9

-- | Convert a 'Word8' to a 'NumPlayers'.
numPlayersFromWord8 :: Word8 -> Maybe NumPlayers
numPlayersFromWord8 2 = Just TwoPlayers
numPlayersFromWord8 3 = Just ThreePlayers
numPlayersFromWord8 4 = Just FourPlayers
numPlayersFromWord8 5 = Just FivePlayers
numPlayersFromWord8 6 = Just SixPlayers
numPlayersFromWord8 7 = Just SevenPlayers
numPlayersFromWord8 8 = Just EightPlayers
numPlayersFromWord8 9 = Just NinePlayers
numPlayersFromWord8 10 = Just TenPlayers
numPlayersFromWord8 _ = Nothing

-- | WARNING: The incoming 'Integral' is downcast to a 'Word8'
--
-- >>> mkNumPlayers 2
-- Just TwoPlayers
-- >>> mkNumPlayers 5
-- Just FivePlayers
-- >>> mkNumPlayers 9
-- Just NinePlayers
mkNumPlayers :: Integral a => a -> Maybe NumPlayers
mkNumPlayers num | num >= 2 && num <= 9 = numPlayersFromWord8 $ fromIntegral num
mkNumPlayers _ = Nothing

-- | >>> allPossiblePositions
-- [UTG,UTG1,UTG2,UTG3,UTG4,UTG5,UTG6,BU,SB,BB]
allPossiblePositions :: [Position]
allPossiblePositions = enumerate

-- | >>> allPositions <$> enumerate
-- [[BU,BB],[BU,SB,BB],[UTG,BU,SB,BB],[UTG,UTG1,BU,SB,BB],[UTG,UTG1,UTG2,BU,SB,BB],[UTG,UTG1,UTG2,UTG3,BU,SB,BB],[UTG,UTG1,UTG2,UTG3,UTG4,BU,SB,BB],[UTG,UTG1,UTG2,UTG3,UTG4,UTG5,BU,SB,BB],[UTG,UTG1,UTG2,UTG3,UTG4,UTG5,UTG6,BU,SB,BB]]
allPositions :: NumPlayers -> [Position]
allPositions = \case
  TwoPlayers -> [BU, BB]
  ThreePlayers -> positions 3
  FourPlayers -> positions 4
  FivePlayers -> positions 5
  SixPlayers -> positions 6
  SevenPlayers -> positions 7
  EightPlayers -> positions 8
  NinePlayers -> positions 9
  TenPlayers -> positions 10
  where
    buThruBb = [BU, SB, BB]
    positions num = take (num - 3) [UTG .. UTG6] ++ buThruBb

-- | >>> getPreflopOrder TwoPlayers
-- [BU,BB]
-- >>> getPreflopOrder SixPlayers
-- [UTG,UTG1,UTG2,BU,SB,BB]
-- >>> getPreflopOrder NinePlayers
-- [UTG,UTG1,UTG2,UTG3,UTG4,UTG5,BU,SB,BB]
getPreflopOrder :: NumPlayers -> [Position]
getPreflopOrder = allPositions

-- | >>> buttonPosition
-- BU
buttonPosition :: Position
buttonPosition = BU

-- | >>> bigBlindPosition
-- WAS with actual type ‘Position’
-- NOW BB
bigBlindPosition :: Position
bigBlindPosition = BB

-- | >>> smallBlindPosition TwoPlayers
-- BU
-- >>> smallBlindPosition ThreePlayers
-- SB
-- >>> smallBlindPosition <$> enumerate
-- [BU,SB,SB,SB,SB,SB,SB,SB,SB]
smallBlindPosition :: NumPlayers -> Position
smallBlindPosition TwoPlayers = BU
smallBlindPosition _ = SB

-- | >>> getPostFlopOrder
-- [SB,BB,UTG,UTG1,UTG2,UTG3,UTG4,UTG5,UTG6,BU]
getPostFlopOrder :: [Position]
getPostFlopOrder = SB : BB : [UTG .. BU]

-- | Sort a list of positions acccording to preflop ordering
--
-- >>> sortPreflop (allPositions TwoPlayers)
-- [BU,BB]
-- >>> sortPreflop (allPositions ThreePlayers)
-- [BU,SB,BB]
-- >>> sortPreflop (allPositions SixPlayers)
-- [UTG,UTG1,UTG2,BU,SB,BB]
-- >>> sortPreflop (allPositions TenPlayers)
-- [UTG,UTG1,UTG2,UTG3,UTG4,UTG5,UTG6,BU,SB,BB]
sortPreflop :: [Position] -> [Position]
sortPreflop ps = filter (`elem` ps) enumerate

-- | Sort a list of positions acccording to postflop ordering
--
-- >>> sortPostflop (allPositions TwoPlayers)
-- [BB,BU]
-- >>> sortPostflop (allPositions ThreePlayers)
-- [SB,BB,BU]
-- >>> sortPostflop (allPositions SixPlayers)
-- [SB,BB,UTG,UTG1,UTG2,BU]
-- >>> sortPostflop (allPositions TenPlayers)
-- [SB,BB,UTG,UTG1,UTG2,UTG3,UTG4,UTG5,UTG6,BU]
sortPostflop :: [Position] -> [Position]
sortPostflop ps = filter (`elem` ps) getPostFlopOrder

-- | Is a player hero or villain. Hero in poker means that the hand is from
-- the hero player's perspective.
data IsHero = Hero | Villain
  deriving (Read, Show, Eq, Ord, Enum, Bounded)

-- | A player's seat number at a poker table.
newtype Seat = Seat {_seat :: Int} deriving (Read, Show, Eq, Ord, Num)

-- | Total amount of money in the 'Pot'.
newtype Pot b = Pot {_pot :: b}
  deriving (Show, Eq, Ord, Num, Functor, Pretty, Semigroup, Monoid)

-- | Amount of money in a player's stack (not having been bet).
newtype Stack b = Stack {_stack :: b}
  deriving (Show, Eq, Ord, Num, Functor, Pretty, Semigroup)

-- | The state of a game with respect to cards turned and betting rounds.
data Board where
  RiverBoard :: !Card -> !Board -> Board
  TurnBoard :: !Card -> !Board -> Board
  FlopBoard :: (Card, Card, Card) -> !Board -> Board
  PreFlopBoard :: !Board -> Board
  InitialTable ::
    -- | Round where post actions occur.
    Board
  deriving (Eq, Ord, Show)

-- | Amount of money needed to join a game.
newtype Stake b = Stake {_stake :: b}
  deriving (Read, Show, Eq, Functor, Ord, Pretty)

-- | A bet done a player pre- or post-flop.
--
-- WARNING: Unstable API
data BetAction t
  = Call !t
  | Raise
      { raiseBy :: !t, -- TODO remove?
        raiseTo :: !t
      }
  | -- TODO remove AllInRaise
    AllInRaise
      { amountRaisedAI :: !t, -- TODO remove?
        raisedAITo :: !t
      }
  | Bet !t
  | -- TODO remove AllIn
    AllIn !t
  | Fold
  | Check
  deriving (Read, Show, Eq, Ord, Functor, Data, Typeable)
