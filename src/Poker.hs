-- | Datatypes and supporting infrastructure for poker computation.
module Poker
  ( -- * Usage
    -- $usage

    -- * Overview
    -- $overview

    -- * Cards
    Rank (..),
    allRanks,
    Suit (..),
    allSuits,
    suitToUnicode,
    suitFromUnicode,
    Card (..),
    allCards,

    -- * Hole cards
    Hole (..),
    mkHole,
    allHoles,
    ShapedHole (..),
    mkPair,
    mkOffsuit,
    mkSuited,
    allShapedHoles,
    holeToShapedHole,
    Deck,
    freshDeck,
    unsafeDeck,
    shapedHoleToHoles,
    rankToChr,
    chrToRank,
    suitToChr,
    chrToSuit,
    cardToShortTxt,
    cardFromShortTxt,
    shapedHoleToShortTxt,
    holeToShortTxt,
    unsafeOffsuit,
    unsafeSuited,
    unsafeHole,
    holeFromShortTxt,

    -- * Game
    -- $game
    Position (..),
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

    -- * Amount
    -- $amount
    Amount (unAmount),
    unsafeAmount,
    IsBet (..),
    mkAmount,
    BigBlind (..),
    bigBlindToDense,

    -- * Range
    Freq (..),
    Range (..),
    getDecisionFreqRange,
    holdingRangeToShapedRange,
    addHoleToShapedRange,
  )
where

import Poker.Amount
import Poker.Cards
import Poker.Game
import Poker.Range

-- $usage
-- >>> import Poker
-- >>> Just h = mkHole (Card Ace Club) (Card Two Diamond)
-- >>> holeToShortTxt h
-- "Ac2d"
