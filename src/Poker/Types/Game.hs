{-# LANGUAGE RecordWildCards #-}
module Poker.Types.Game where
  -- ( Action (..)
  -- , TableActionValue (..)
  -- , DealerAction (..)
  -- , TableAction (..)
  -- , BetAction (..)
  -- , PlayerAction (..)
  -- , IsHero (..)
  -- , Card (..)
  -- , Rank (..)
  -- , Suit (..)
  -- , Shape (..)
  -- , Position (..)
  -- , HandInfo (..)
  -- , Hand (..)
  --   , handInfo
  --   , handNetwork
  --   , handStakes
  --   , handPlayerMap
  --   , handSeatMap
  --   , handActions
  --   , handText
  -- , Deck (..)
  -- , ActionIx (..)
  -- , IxRange (..)
  -- , Player (..)
  --   , name
  --   , playerPosition
  --   , playerHolding
  --   , stack
  --   , seat
  -- , Holding (..)
  -- , ShapedHand (..)
  -- , Network (..)
  -- , Seat
  -- , GameType (..)
  -- , IsAction (..)
  -- ) where

import Poker.Types.Cards
import Control.Lens
import Data.Data
import Data.Map (Map)
import Data.Time.LocalTime (LocalTime (..))
import GHC.Generics

data Position = UTG | UTG1 | UTG2 | BU | SB | BB
  deriving (Read, Show, Enum, Eq, Ord, Data, Typeable, Generic)

data GameType = Zone | Cash
  deriving (Show, Eq, Ord, Read, Generic)

data IsHero = Hero | Villain
  deriving (Read, Show, Eq, Ord, Data, Typeable, Generic)

type Seat = Int

data Network = Bovada | PokerStars | Unknown
  deriving (Read, Show, Enum, Eq, Ord, Generic)

data BetAction
  = Call Double
  | Raise
      { amountRaised :: Double,
        raisedTo :: Double
      }
  | AllInRaise
      { amountRaisedAI :: Double,
        raisedAITo :: Double
      }
  | Bet Double
  | AllIn Double
  | Fold
  | FoldTimeout
  | Check
  | CheckTimeOut
  | OtherAction -- TODO remove
  deriving (Read, Show, Eq, Ord, Data, Typeable, Generic)

data PlayerAction
  = PlayerAction
      { position :: Position,
        action :: BetAction,
        isHero :: IsHero
      }
  deriving (Read, Show, Eq, Ord, Data, Typeable, Generic)

data TableAction
  = TableAction Position TableActionValue
  | UnknownAction
  deriving (Read, Show, Eq, Ord, Data, Typeable, Generic)

data TableActionValue
  = Post Double
  | PostDead Double
  | Leave
  | Deposit Double
  | Enter
  | SitOut
  | SitDown
  | Showdown [Card] String
  | Muck [Card] String
  | Rejoin
  | Return Double
  | Result Double
  deriving (Read, Show, Ord, Eq, Data, Typeable, Generic)

-- TODO Fix the below to become the above
data DealerAction
  = PlayerDeal
  | FlopDeal (Card, Card, Card)
  | TurnDeal Card
  | RiverDeal Card
  deriving (Read, Show, Eq, Ord, Data, Typeable, Generic)


data Action
  = MkPlayerAction PlayerAction
  | MkDealerAction DealerAction
  | MkTableAction TableAction
  deriving (Read, Show, Eq, Ord, Data, Typeable, Generic)

class IsAction a where
  toAction :: a -> Action

instance IsAction PlayerAction where
  toAction = MkPlayerAction

instance IsAction TableAction where
  toAction = MkTableAction

instance IsAction DealerAction where
  toAction = MkDealerAction

data Player
  = Player
      { _name :: Maybe String,
        _playerPosition :: Maybe Position,
        _playerHolding :: Maybe Holding,
        _stack :: Double,
        _seat :: Seat
      }
  deriving (Show, Eq, Ord, Generic)

data Hand
  = Hand
      { _handID :: Int,
        _handNetwork :: Network,
        _handTy :: GameType,
        _handTime :: LocalTime,
        _handStakes :: Double,
        _handPlayerMap :: Map Seat Player,
        _handSeatMap :: Map Position Seat,
        _handActions :: [Action],
        _handText :: String
      }
  deriving (Show, Eq, Ord, Generic)

data Board where
  RiverBoard :: Card -> Board -> Board
  TurnBoard :: Card -> Board -> Board
  FlopBoard :: (Card, Card, Card) -> Board -> Board
  PreFlopBoard :: Board -> Board
  InitialTable :: Board
  deriving (Eq, Ord)

newtype Stake = Stake { getStake :: Double }
  deriving (Read, Show, Eq, Ord)

instance Show Board where
  show InitialTable = "InitialBoard"
  show (PreFlopBoard _) = "PreFlop"
  show (FlopBoard (c1, c2, c3) _) = "Flop is: " ++ show c1 ++ show c2 ++ show c3
  show (TurnBoard card flop) = "Turn is: " ++ show card ++ show flop
  show (RiverBoard card flop) = "River is: " ++ show card ++ show flop

makeLenses ''Player
makeLenses ''Hand

