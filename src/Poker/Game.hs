{-# LANGUAGE OverloadedStrings #-}

-- TODO fix exports
module Poker.Game where

import Data.Data
import Data.Text (Text)
import Data.Word (Word8)
import Poker.Cards
import Prettyprinter

-- | A player's position in a game of poker.
--
-- Future iterations of this library will use a safer/less-hacky representation
-- for 'Position'
newtype Position = Position Word8
  deriving (Read, Show, Enum, Bounded, Eq, Ord, Data, Typeable)

instance Pretty Position where
  pretty = viaShow

-- TODO fromIntegral should not allow construction of unsupport table size
-- TODO could be an enum? HeadsUp | Three | Four
-- TODO could be a ranged natural?
-- TODO name choice? TableSize?
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

numPlayersToWord8 :: NumPlayers -> Word8
numPlayersToWord8 TwoPlayers = 2
numPlayersToWord8 ThreePlayers = 3
numPlayersToWord8 FourPlayers = 4
numPlayersToWord8 FivePlayers = 5
numPlayersToWord8 SixPlayers = 6
numPlayersToWord8 SevenPlayers = 7
numPlayersToWord8 EightPlayers = 8
numPlayersToWord8 NinePlayers = 9

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

-- | 'Position's are ordered by table order. The first position in the list
-- is the first player to act preflop. The last position in the list is always
-- the big blind.
-- >>> allPositions 6
-- [Position 1,Position 2,Position 3,Position 4,Position 5,Position 6]
allPositions :: NumPlayers -> [Position]
allPositions (numPlayersToWord8 -> num) = Position <$> [1 .. num]

-- |
-- >>> positionToTxt 2 <$> allPositions 2
-- ["BU","BB"]
-- >>> positionToTxt 6 <$> allPositions 6
-- ["LJ","HJ","CO","BU","SB","BB"]
-- >>> positionToTxt 9 <$> allPositions 9
-- ["UTG","UTG1","UTG2","LJ","HJ","CO","BU","SB","BB"]
--
-- TODO Pre-compute, via TH, Position -> Text maps for each NumPlayers, to avoid
-- extra runtime cost
positionToTxt :: NumPlayers -> Position -> Text
positionToTxt (numPlayersToWord8 -> num) (Position pos) =
  let allPositionTexts = ["UTG", "UTG1", "UTG2", "LJ", "HJ", "CO", "BU", "SB", "BB"]
      positionTexts = case num of
        2 -> ["BU", "BB"]
        num' | num' > 2 && num' <= 9 -> drop (9 - fromIntegral num') allPositionTexts
        _ -> error $ "Unexpected NumPlayers value: " <> show num
   in positionTexts !! (fromIntegral pos - 1)

-- >>> positionToTxt 2 <$> getPreflopOrder 2
-- ["BU","BB"]
-- >>> positionToTxt 6 <$> getPreflopOrder 6
-- ["LJ","HJ","CO","BU","SB","BB"]
-- >>> positionToTxt 9 <$> getPreflopOrder 9
-- ["UTG","UTG1","UTG2","LJ","HJ","CO","BU","SB","BB"]
getPreflopOrder :: NumPlayers -> [Position]
getPreflopOrder = allPositions

-- >>> buttonPosition 2
-- Position 1
-- >>> (\numPlayers -> positionToTxt numPlayers $ buttonPosition numPlayers) <$> [2..9]
-- ["BU","BU","BU","BU","BU","BU","BU","BU"]
buttonPosition :: NumPlayers -> Position
buttonPosition (numPlayersToWord8 -> num) = case num of
  2 -> Position 1
  _ -> Position (num - 2)

-- >>> bigBlindPosition 2
-- Position 2
-- >>> (\numPlayers -> positionToTxt numPlayers $ bigBlindPosition numPlayers) <$> [2..9]
-- ["BB","BB","BB","BB","BB","BB","BB","BB"]
bigBlindPosition :: NumPlayers -> Position
bigBlindPosition (numPlayersToWord8 -> num) = Position num

-- >>> positionToTxt 2 <$> getPostFlopOrder 2
-- ["BB","BU"]
-- >>> positionToTxt 3 <$> getPostFlopOrder 3
-- ["SB","BB","BU"]
-- >>> positionToTxt 6 <$> getPostFlopOrder 6
-- ["SB","BB","LJ","HJ","CO","BU"]
-- >>> positionToTxt 9 <$> getPostFlopOrder 9
-- ["SB","BB","UTG","UTG1","UTG2","LJ","HJ","CO","BU"]
getPostFlopOrder :: NumPlayers -> [Position]
getPostFlopOrder numPlayers@(fromIntegral . numPlayersToWord8 -> num) =
  take num
    . drop 1
    . dropWhile (/= buttonPosition numPlayers)
    . cycle
    $ allPositions numPlayers

-- | Sort a list of positions acccording to postflop ordering
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

data IsHero = Hero | Villain
  deriving (Read, Show, Eq, Ord, Enum, Bounded)

newtype Seat = Seat {_unSeat :: Int} deriving (Read, Show, Eq, Ord, Num)

newtype Pot b = Pot {_unPot :: b}
  deriving (Show, Eq, Ord, Num, Functor, Pretty, Semigroup, Monoid)

newtype Stack b = Stack {_unStack :: b}
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

newtype Stake b = Stake {unStake :: b}
  deriving (Read, Show, Eq, Functor, Ord, Pretty)

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
