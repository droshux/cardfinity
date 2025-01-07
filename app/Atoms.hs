{-# LANGUAGE LambdaCase #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Redundant <&>" #-}

module Atoms where

import Control.Monad.Except (MonadTrans (lift), throwError, when)
import Control.Monad.RWS (MonadReader (ask))
import Data.Functor ((<&>))
import GHC.Natural (Natural)
import Types
import Utils (natToInt, playerState, trigger, updatePlayerState)

data SearchType = ForName String | ForFamlily String | ForSpell | ForMonster | ForCard deriving (Ord, Eq)

toPredicate :: SearchType -> Card -> Bool
toPredicate (ForName n) = (n ==) . cardName
toPredicate (ForFamlily f) = elem f . cardFamilies
toPredicate ForCard = const True
toPredicate t = cardElim (const $ t == ForSpell) (const $ t == ForMonster)

instance Show SearchType where
  show ForCard = "card"
  show ForSpell = "spell card"
  show ForMonster = "monster card"
  show (ForName n) = show n
  show (ForFamlily f) = show f ++ " card"

data DestroyCards = Discard Natural SearchType CardLocation | Banish Natural SearchType CardLocation deriving (Eq, Ord)

instance HasScale DestroyCards where
  scale (Discard n _ loc) =
    natToInt n * case loc of
      Graveyard -> 0
      Deck -> -5
      Hand -> -10
      Field -> -15
  scale (Banish n t loc) = scale (Discard n t loc) - (2 * natToInt n)

instance Show DestroyCards where
  show d = case d of
    Discard n t l -> "Discard" ++ helper n t l
    Banish n t l -> "Banish" ++ helper n t l
    where
      helper n t l = concat [" ", show n, " ", show t, if n == 1 then "s" else "", " from the ", show l]

-- TODO: Let players choose specific cards from their hand/field
-- TODO: Filter options that don't match the card type out
-- TODO: Refactor DestroyCards to be (Destroy+Banish)×FindInLocation
-- TODO: Refactor FindInLocation to be Natural×SearchType×CardLocation
instance Requirement DestroyCards where
  testRequirement (Discard 0 _ _) = return True
  testRequirement (Discard n t l) = case l of
    Graveyard -> return True -- Cannot discard from graveyard so just do nothing.
    other ->
      lift playerState <&> toLens other >>= \case
        [] -> do
          when (other == Deck) $ performEffect Deckout
          return False
        (c : cs) -> do
          let toGY ps = ps {graveyard = c : graveyard ps}
          lift $ updatePlayerState (destroyHelper other cs . toGY)
          lift $ trigger OnDiscard c
          testRequirement $ Discard (n - 1) l
  testRequirement (Banish 0 _ _) = return True
  testRequirement (Banish n t l) =
    lift playerState <&> toLens l >>= \case
      [] -> do
        when (l == Deck) $ performEffect Deckout
        return False
      (_ : cs) -> do
        lift $ updatePlayerState $ destroyHelper l cs
        testRequirement $ Banish (n - 1) t l

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
  performEffect _ = lift ask >>= throwError

data FindInLocation = SearchType `IsIn` CardLocation deriving (Ord, Eq)

instance Show FindInLocation where
  show (t `IsIn` l) = "if " ++ show t ++ "is " ++ io ++ "n the " ++ show l
    where
      io = case l of
        Field -> "o"
        _ -> "i"

instance HasScale FindInLocation where
  scale (IsIn _ Deck) = 0
  scale (IsIn _ Field) = -2
  scale (IsIn _ _) = -1

instance Requirement FindInLocation where
  testRequirement (IsIn t l) = playerState <&> any (toPredicate t) . toLens l
