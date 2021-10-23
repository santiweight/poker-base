{-# LANGUAGE OverloadedStrings #-}

-- | Representation of a game of holdem, including table structure, positioning, pot and betting state.
module Poker.Game
  ( Position (..),
    NumPlayers (..),
    numPlayersToWord8,
    numPlayersFromWord8,
    mkNumPlayers,
    allPositions,
    positionToTxt,
    getPreflopOrder,
    buttonPosition,
    bigBlindPosition,
    getPostFlopOrder,
    sortPostflop,
    Seat (..),
    Pot (..),
    Stack (..),
    Stake (..),
  )
where

import Data.Data
import Data.Text (Text)
import Data.Word (Word8)
import Poker.Cards
import Prettyprinter

-- | A player's 'Position' in a game of poker.
--
-- 'Position's are ordered by table order (clockwise). The smallest 'Position', @Position 0@,
-- is the first player to act preflop. The largest 'Position' is always the big blind.
--
-- >>> allPositions SixPlayers
-- [Position 0,Position 1,Position 2,Position 3,Position 4,Position 5]
-- >>> positionToTxt SixPlayers <$> allPositions SixPlayers
-- ["LJ","HJ","CO","BU","SB","BB"]
-- >>> positionToTxt NinePlayers <$> allPositions NinePlayers
-- ["UTG","UTG1","UTG2","LJ","HJ","CO","BU","SB","BB"]
--
-- The API for 'Position' is unstable. We are open to better ideas :)
newtype Position = Position Word8
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
  deriving (Enum, Eq, Ord)

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
numPlayersFromWord8 _ = Nothing

-- | WARNING: The incoming 'Integral' is downcast to a 'Word8'
mkNumPlayers :: Integral a => a -> Maybe NumPlayers
mkNumPlayers num | num >= 2 && num <= 9 = numPlayersFromWord8 $ fromIntegral num
mkNumPlayers _ = Nothing

-- | >>> allPositions SixPlayers
-- [Position 0,Position 1,Position 2,Position 3,Position 4,Position 5]
allPositions :: NumPlayers -> [Position]
allPositions (numPlayersToWord8 -> num) = Position <$> [0 .. num - 1]

-- | >>> positionToTxt TwoPlayers <$> allPositions TwoPlayers
-- ["BU","BB"]
-- >>> positionToTxt SixPlayers <$> allPositions SixPlayers
-- ["LJ","HJ","CO","BU","SB","BB"]
-- >>> positionToTxt NinePlayers <$> allPositions NinePlayers
-- ["UTG","UTG1","UTG2","LJ","HJ","CO","BU","SB","BB"]
positionToTxt :: NumPlayers -> Position -> Text
positionToTxt (numPlayersToWord8 -> num) (Position pos) =
  let allPositionTexts = ["UTG", "UTG1", "UTG2", "LJ", "HJ", "CO", "BU", "SB", "BB"]
      positionTexts = case num of
        2 -> ["BU", "BB"]
        num' | num' > 2 && num' <= 9 -> drop (9 - fromIntegral num') allPositionTexts
        _ -> error $ "Unexpected NumPlayers value: " <> show num
   in positionTexts !! fromIntegral pos

-- | >>> positionToTxt TwoPlayers <$> getPreflopOrder TwoPlayers
-- ["BU","BB"]
-- >>> positionToTxt SixPlayers <$> getPreflopOrder SixPlayers
-- ["LJ","HJ","CO","BU","SB","BB"]
-- >>> positionToTxt NinePlayers <$> getPreflopOrder NinePlayers
-- ["UTG","UTG1","UTG2","LJ","HJ","CO","BU","SB","BB"]
getPreflopOrder :: NumPlayers -> [Position]
getPreflopOrder = allPositions

-- | >>> buttonPosition TwoPlayers
-- Position 0
-- >>> (\numPlayers -> positionToTxt numPlayers $ buttonPosition numPlayers) <$> enumFromTo TwoPlayers NinePlayers
-- ["BU","BU","BU","BU","BU","BU","BU","BU"]
buttonPosition :: NumPlayers -> Position
buttonPosition (numPlayersToWord8 -> num) = case num of
  2 -> Position 0
  _ -> Position (num - 3)

-- | >>> bigBlindPosition TwoPlayers
-- Position 1
-- >>> (\numPlayers -> positionToTxt numPlayers $ bigBlindPosition numPlayers) <$> enumFromTo TwoPlayers NinePlayers
-- ["BB","BB","BB","BB","BB","BB","BB","BB"]
bigBlindPosition :: NumPlayers -> Position
bigBlindPosition (numPlayersToWord8 -> num) = Position (num - 1)

-- | >>> positionToTxt TwoPlayers <$> getPostFlopOrder TwoPlayers
-- ["BB","BU"]
-- >>> positionToTxt ThreePlayers <$> getPostFlopOrder ThreePlayers
-- ["SB","BB","BU"]
-- >>> positionToTxt SixPlayers <$> getPostFlopOrder SixPlayers
-- ["SB","BB","LJ","HJ","CO","BU"]
-- >>> positionToTxt NinePlayers <$> getPostFlopOrder NinePlayers
-- ["SB","BB","UTG","UTG1","UTG2","LJ","HJ","CO","BU"]
getPostFlopOrder :: NumPlayers -> [Position]
getPostFlopOrder numPlayers@(fromIntegral . numPlayersToWord8 -> num) =
  take num
    . drop 1
    . dropWhile (/= buttonPosition numPlayers)
    . cycle
    $ allPositions numPlayers

-- | Sort a list of positions acccording to postflop ordering
--
-- >>> positionToTxt TwoPlayers <$> sortPostflop TwoPlayers (allPositions TwoPlayers)
-- ["BB","BU"]
-- >>> positionToTxt ThreePlayers <$> sortPostflop ThreePlayers (allPositions ThreePlayers)
-- ["SB","BB","BU"]
-- >>> positionToTxt SixPlayers <$> sortPostflop SixPlayers (allPositions SixPlayers)
-- ["SB","BB","LJ","HJ","CO","BU"]
-- >>> positionToTxt NinePlayers <$> sortPostflop NinePlayers (allPositions NinePlayers)
-- ["SB","BB","UTG","UTG1","UTG2","LJ","HJ","CO","BU"]
sortPostflop :: NumPlayers -> [Position] -> [Position]
sortPostflop num ps = filter (`elem` ps) $ getPostFlopOrder num

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
  InitialTable :: Board -- ^ Round where post actions occur.
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
