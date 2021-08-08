{-# LANGUAGE DerivingVia #-}
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
import Test.QuickCheck (Arbitrary (arbitrary, shrink), arbitraryBoundedEnum)
import Data.List (sort)
import Data.Text.Prettyprint.Doc (Pretty (pretty), (<+>))

data Position = UTG | UTG1 | UTG2 | BU | SB | BB
  deriving (Read, Show, Enum, Bounded, Eq, Ord, Data, Typeable, Generic)

-- | Sort a list of positions according to preflop ordering
-- >>> sortPreflop $ [BB,BU,UTG1,SB,UTG,UTG2]
-- [UTG,UTG1,UTG2,BU,SB,BB]
sortPreflop :: [Position] -> [Position]
sortPreflop = fmap toEnum . sort . fmap fromEnum

-- | Sort a list of positions acccording to postflop ordering
-- >>> sortPostflop $ [BB,BU,UTG1,SB,UTG,UTG2]
-- [SB,BB,UTG,UTG1,UTG2,BU]
sortPostflop :: [Position] -> [Position]
sortPostflop = fmap (toEnum . fromPostFlopOrder) . sort . fmap
  (toPostFlopOrder . fromEnum)
 where
  fromPostFlopOrder = flip mod 6 . (+ 4)
  toPostFlopOrder   = flip mod 6 . (+ 2)


data GameType = Zone | Cash
  deriving (Show, Eq, Ord, Read, Enum, Generic)

data IsHero = Hero | Villain
  deriving (Read, Show, Eq, Ord, Enum, Bounded, Data, Typeable, Generic)

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
  | Check
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

instance Pretty Board where
  pretty InitialTable = pretty "InitialBoard"
  pretty (PreFlopBoard _) = pretty "PreFlop"
  pretty (FlopBoard (c1, c2, c3) _) = pretty "Flop is: " <+> pretty c1 <+> pretty c2 <+> pretty c3
  pretty (TurnBoard card flop) = pretty "Turn is: " <+> pretty card <+> pretty flop
  pretty (RiverBoard card flop) = pretty "River is: " <+> pretty card <+> pretty flop

makeLenses ''Player
makeLenses ''Hand