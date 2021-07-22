{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Poker.Types.Game where

import Poker.Types.Cards
import Control.Lens ( makeLenses )
import Data.Data
import Data.Map (Map)
import Data.Time.LocalTime (LocalTime (..))
import GHC.Generics
import Algebra.PartialOrd (PartialOrd)

data Position = UTG | UTG1 | UTG2 | BU | SB | BB
  deriving (Read, Show, Enum, Eq, Ord, Data, Typeable, Generic)

data GameType = Zone | Cash
  deriving (Show, Eq, Ord, Read, Generic)

data IsHero = Hero | Villain
  deriving (Read, Show, Eq, Ord, Data, Typeable, Generic)

type Seat = Int

data Network = Bovada | PokerStars | Unknown
  deriving (Read, Show, Enum, Eq, Ord, Generic)

newtype PotSize b = PotSize b
  deriving (Show, Eq, Ord, Num, Functor, PartialOrd)

newtype StackSize b = StackSize b
  deriving (Show, Eq, Ord, Num)

data BetAction t
  = Call t
  | Raise
      { amountRaised :: t,
        raisedTo :: t
      }
  | AllInRaise
      { amountRaisedAI :: t,
        raisedAITo :: t
      }
  | Bet t
  | AllIn t
  | Fold
  | FoldTimeout
  | Check
  | CheckTimeOut
  | OtherAction -- TODO remove
  deriving (Read, Show, Eq, Ord, Data, Typeable, Generic, Functor)

data PlayerAction t
  = PlayerAction
      { position :: Position,
        action :: BetAction t,
        isHero :: IsHero
      }
  deriving (Read, Show, Eq, Ord, Data, Typeable, Generic, Functor)

data TableAction t
  = TableAction Position (TableActionValue t)
  | UnknownAction
  deriving (Read, Show, Eq, Ord, Data, Typeable, Generic, Functor)

data TableActionValue t
  = Post t
  | PostDead t
  | Leave
  | Deposit t
  | Enter
  | SitOut
  | SitDown
  | Showdown [Card] String
  | Muck [Card] String
  | Rejoin
  | Return t
  | Result t
  deriving (Read, Show, Ord, Eq, Data, Typeable, Generic, Functor)

-- TODO Fix the below to become the above
data DealerAction
  = PlayerDeal
  | FlopDeal (Card, Card, Card)
  | TurnDeal Card
  | RiverDeal Card
  deriving (Read, Show, Eq, Ord, Data, Typeable, Generic)


data Action t
  = MkPlayerAction (PlayerAction t)
  | MkDealerAction DealerAction
  | MkTableAction (TableAction t)
  deriving (Read, Show, Eq, Ord, Data, Typeable, Generic, Functor)

data Player t
  = Player
      { _name :: Maybe String,
        _playerPosition :: Maybe Position,
        _playerHolding :: Maybe Hand,
        _stack :: t,
        _seat :: Seat
      }
  deriving (Show, Eq, Ord, Generic, Functor)

data HandHistory t
  = HandHistory
      { _handID :: Int,
        _handNetwork :: Network,
        _handTy :: GameType,
        _handTime :: LocalTime,
        _handStakes :: Stake t,
        _handPlayerMap :: Map Seat (Player t),
        _handSeatMap :: Map Position Seat,
        _handActions :: [Action t],
        _handText :: String
      }
  deriving (Show, Eq, Ord, Generic, Functor)

data Board where
  RiverBoard :: Card -> Board -> Board
  TurnBoard :: Card -> Board -> Board
  FlopBoard :: (Card, Card, Card) -> Board -> Board
  PreFlopBoard :: Board -> Board
  InitialTable :: Board
  deriving (Eq, Ord)

newtype Stake b = Stake { getStake :: b }
  deriving (Read, Show, Eq, Functor, Ord)

instance Show Board where
  show InitialTable = "InitialBoard"
  show (PreFlopBoard _) = "PreFlop"
  show (FlopBoard (c1, c2, c3) _) = "Flop is: " ++ show c1 ++ show c2 ++ show c3
  show (TurnBoard card flop) = "Turn is: " ++ show card ++ show flop
  show (RiverBoard card flop) = "River is: " ++ show card ++ show flop

makeLenses ''Player
makeLenses ''Hand
