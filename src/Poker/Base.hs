{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}

-- BIG IMPORTANT TODO GET RID OF EQ INSTANCE FOR INDEX

{-# LANGUAGE AllowAmbiguousTypes #-}
module Poker.Base
  ( module Poker.Types
  , inIndex
  ) where

import           Data.List                      ( sort )
import           Poker.Types

-- TODO test/fix
inIndex :: forall b . (Ord b, Num b) => ActionIx b -> BetAction b -> Bool
inIndex = go
 where
  within' = within @b
  go AnyIx                  _               = True
  go CheckIx                Check           = True
  go (RaiseIx        range) (Raise from to) = within' (to - from) range
  go (RaiseOrAllInIx range) (Raise from to) = within' (to - from) range
  go (RaiseOrAllInIx range) (AllIn bet    ) = within' bet range
  go (AllInIx        range) (AllIn allIn  ) = within' allIn range
  go (BetIx          range) (Bet   bet    ) = within' bet range
  go CallIx                 (Call  _      ) = True
  go FoldIx                 Fold            = True
  go _                      _               = False