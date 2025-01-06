{-# LANGUAGE LambdaCase #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Redundant <&>" #-}

module Atoms where

import Control.Monad.Except (throwError, when)
import Control.Monad.RWS (asks)
import Data.Functor ((<&>))
import GHC.Natural (Natural)
import Types
import Utils (natToInt, playerState, showCardCount, trigger, updatePlayerState)

data DestroyCards = Discard Natural CardLocation | Banish Natural CardLocation deriving (Eq, Ord)

instance HasScale DestroyCards where
  scale (Discard n loc) =
    natToInt n * case loc of
      Graveyard -> 0
      Deck -> -5
      Hand -> -10
      Field -> -15
  scale (Banish n loc) = scale (Discard n loc) - (2 * natToInt n)

instance Show DestroyCards where
  show d = case d of
    Discard n l -> "Discard" ++ helper n l
    Banish n l -> "Banish" ++ helper n l
    where
      helper n l = concat [" ", showCardCount n, " from the ", show l]

instance Requirement DestroyCards where
  testRequirement (Discard 0 _) = return True
  testRequirement (Discard n l) = case l of
    Graveyard -> return True -- Cannot discard from graveyard so just do nothing.
    other ->
      playerState <&> toLens other >>= \case
        [] -> do
          when (other == Deck) $ performEffect Deckout
          return False
        (c : cs) -> do
          let toGY ps = ps {graveyard = c : graveyard ps}
          updatePlayerState (destroyHelper other cs . toGY)
          trigger OnDiscard c
          testRequirement $ Discard (n - 1) l
  testRequirement (Banish 0 _) = return True
  testRequirement (Banish n l) =
    playerState <&> toLens l >>= \case
      [] -> do
        when (l == Deck) $ performEffect Deckout
        return False
      (_ : cs) -> do
        updatePlayerState $ destroyHelper l cs
        testRequirement $ Banish (n - 1) l

destroyHelper :: CardLocation -> [Card] -> PlayerState -> PlayerState
-- We don't need to distinguish banish VS discard becaue it's not called for
-- banish Graveyard
destroyHelper Graveyard cs ps = ps {graveyard = cs}
destroyHelper Deck cs ps = ps {deck = cs}
destroyHelper Hand cs ps = ps {hand = cs}
destroyHelper Field cs ps = ps {field = cs}

instance Effect DestroyCards where
  performEffect = (<&> const ()) . testRequirement

data Deckout = Deckout deriving (Eq, Ord)

instance Show Deckout where
  show = const "DECKOUT"

instance HasScale Deckout where
  scale = const (-50)

instance Effect Deckout where
  -- Cancells everything early and ends the game
  performEffect _ = asks currentPlayer >>= throwError

data SearchType = ForName String | ForFamlily String | ForSpell | ForMonster
