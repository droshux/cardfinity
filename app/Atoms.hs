{-# LANGUAGE LambdaCase #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Redundant <&>" #-}

module Atoms where

import Control.Monad.Except (MonadIO (liftIO), MonadTrans (lift), throwError)
import Control.Monad.RWS (MonadReader (ask), unless)
import Data.Functor ((<&>))
import Data.List (findIndex)
import GHC.Natural (Natural)
import Types
import Utils

data SearchType = ForName String | ForFamily String | ForSpell | ForMonster | ForCard deriving (Ord)

instance Eq SearchType where
  (==) (ForName _) (ForName _) = True
  (==) (ForFamily _) (ForFamily _) = True
  (==) ForSpell ForSpell = True
  (==) ForMonster ForMonster = True
  (==) ForCard ForCard = True
  (==) _ _ = False

toPredicate :: SearchType -> Card -> Bool
toPredicate (ForName n) = (n ==) . cardName
toPredicate (ForFamily f) = elem f . cardFamilies
toPredicate ForCard = const True
toPredicate t = cardElim (const $ t == ForSpell) (const $ t == ForMonster)

instance Show SearchType where
  show ForCard = "card"
  show ForSpell = "spell card"
  show ForMonster = "monster card"
  show (ForName n) = show n
  show (ForFamily f) = show f ++ " card"

data FindCards = FindCards Natural SearchType CardLocation deriving (Ord, Eq)

instance Show FindCards where
  show (FindCards n t l) =
    concat
      [ show n,
        " ",
        show t,
        if n == 1 then " " else "s ",
        case l of Field -> "o"; _ -> "i",
        "n the ",
        show l
      ]

instance HasScale FindCards where
  scale (FindCards _ _ Deck) = 0
  scale (FindCards n _ Field) = -(2 * natToInt n)
  scale (FindCards n _ _) = -natToInt n

instance Requirement FindCards where
  testRequirement (FindCards n t l) = lift $ playerState <&> h
    where
      h = (>= natToInt n) . length . filter (toPredicate t) . toLens l

data DestroyType = Discard | Banish deriving (Eq, Ord, Show)

data DestroyCards = Destroy DestroyType FindCards deriving (Eq, Ord)

instance HasScale DestroyCards where
  scale (Destroy Discard (FindCards n _ loc)) =
    natToInt n * case loc of
      Graveyard -> 0
      Deck -> -5
      Hand -> -10
      Field -> -15
  scale (Destroy Banish f) = scale (Destroy Discard f) - case f of (FindCards n _ _) -> 2 * natToInt n

instance Show DestroyCards where
  show (Destroy d f) = show d ++ " " ++ show f

instance Requirement DestroyCards where
  -- Cannot discard from graveyard
  testRequirement (Destroy _ (FindCards 0 _ _)) = return True
  -- No effect => Always succeeds, Base Case => Always succeeds
  testRequirement (Destroy Discard (FindCards _ _ Graveyard)) = return False
  testRequirement (Destroy d (FindCards n t l)) = case l of
    Deck ->
      lift playerState <&> filter (toPredicate t) . deck >>= \case
        [] -> performEffect Deckout >> return False
        cs -> case findIndex (toPredicate t) cs of
          Nothing -> return False
          Just i -> do
            lift $ updatePlayerState $ \p -> p {deck = deck p `without` i}
            handleDiscard $ cs !! i
            moveOn
    other ->
      lift playerState <&> filter (toPredicate t) . toLens other >>= lift . selectFromList >>= \case
        Nothing -> cancelDestruction
        Just (i, c) -> do
          upWithout other i
          handleDiscard c
          moveOn
    where
      moveOn = testRequirement $ Destroy d $ FindCards (n - 1) t l
      cancelDestruction = do
        liftIO $ putStrLn ("Cancelled " ++ show d ++ "ing.")
        return False
      handleDiscard c = unless (d == Banish) $ lift $ do
        updatePlayerState $ \p -> p {graveyard = c : graveyard p}
        trigger OnDiscard c
      upWithout loc i = lift $ updatePlayerState $ \p -> case loc of
        Hand -> p {hand = hand p `without` i}
        Field -> p {field = field p `without` i}
        _ -> p

data Deckout = Deckout deriving (Eq, Ord)

instance Show Deckout where
  show = const "DECKOUT"

instance HasScale Deckout where
  scale = const (-50)

instance Effect Deckout where
  -- Cancells everything early and ends the game
  performEffect _ = lift ask >>= throwError
