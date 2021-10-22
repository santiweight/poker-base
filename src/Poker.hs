{-# LANGUAGE PatternSynonyms #-}

-- | Datatypes and supporting infrastructure for poker computation.
--
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
    Hole,
    pattern Hole,
    mkHole,
    allHoles,
    ShapedHole (Pair),
    pattern Offsuit,
    pattern Suited,
    mkPair,
    mkOffsuit,
    mkSuited,
    allShapedHoles,
    holeToShapedHole,
    Deck,
    pattern Deck,
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
    pattern Amount,
    unsafeMkAmount,
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
import Poker.BigBlind
import Poker.Cards
import Poker.Game
import Poker.Range

-- $usage
-- >>> import Poker
-- >>> h = Hand [Card Ace Club, Card Two Diamond]
-- >>> pretty h
-- >>> Ac2d
