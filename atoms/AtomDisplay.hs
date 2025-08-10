{-# OPTIONS_GHC -Wno-orphans #-}

module AtomDisplay where

import Atoms (Condition (..), Effect (..), FindCards (..), SearchType (..))
import Utils (showFold)

instance Show SearchType where
  show ForCard = "card"
  show ForSpell = "spell"
  show ForMonster = "monster"
  show (ForName n) = show n
  show (ForFamily f) = show f ++ " card"

instance Show FindCards where
  show f = case f of
    FindCardsHand n t -> part1 n t ++ "in the hand"
    FindCardsField n t -> part1 n t ++ "on the field"
    where
      part1 n t = concat [show n, " ", show t, if n == 1 then " " else "s "]

instance Show Condition where
  show (Destroy d f) = show d ++ " " ++ show f
  show DiscardSelf = "Discard the top card of the deck"
  show (TakeDamage n isTrue) = concat ["Take ", show n, if isTrue then " True" else "", " damage"]
  show (HealOpponent n) = "Heal the opponent for " ++ show n ++ " damage"
  show (Pop n) =
    concat
      [ "Banish the top ",
        if n == 1 then "" else show n ++ " ",
        "card",
        if n == 1 then "" else "s",
        " of the graveyard"
      ]
  show (YouMay cond) = "You can " ++ show cond
  show (Choose cs) = "Choose one of (" ++ showFold " or " cs ++ ")"

instance Show Effect where
  show (DestroyEnemy d f) = replaceLast " the " " the enemy " $ show (Destroy d f)
    where
      replaceLast b a s = reverse $ replaceFirst (reverse b) (reverse a) (reverse s)
      replaceFirst _ _ [] = []
      replaceFirst b a (c : cs) = case stripPrefix b (c : cs) of
        Just cs' -> a ++ cs'
        Nothing -> c : replaceFirst b a cs
  show DiscardEnemy = "Discard the top card of the opponent's deck"
  show (DealDamage n isTrue) = concat ["Deal ", show n, if isTrue then " True" else "", " damage"]
  show (Heal n) = "Heal " ++ show n ++ " damage"
  show DECKOUT = "DECKOUT"
  show (Draw n) = "Draw " ++ show n ++ " card" ++ if n == 1 then " " else "s"
  show (Peek n) =
    concat
      [ "See the top ",
        if n == 1 then "" else show n ++ " ",
        "card",
        if n == 1 then "" else "s",
        " of the deck"
      ]
  show (Scry n) =
    concat
      [ "Banish the top ",
        if n == 1 then "" else show n ++ " ",
        "card",
        if n == 1 then "" else "s",
        " of the graveyard"
      ]
  show (Optional e) = "You may " ++ show e
  show (ChooseEffect es) = "Choose one of (" ++ showFold " or " es ++ ")"
  show (Attack piercing) =
    "Attack with this monster"
      ++ if piercing then " dealing piercing damage" else ""
  show (Play t) = "Play a " ++ show t
  show (Search (SearchFor t)) = "Search the deck for a " ++ show t
  show (Search (DrillFor t)) = "Drill the deck for a " ++ show t
  show (Attach t) = "Attach a " ++ show t ++ " from your hand to this card"
  show (Buff by forItself) =
    (++ show by) $
      if forItself
        then "Increase this card's power by "
        else "Increase a card on the Field's power by "
  show (AsEffect cond) = show cond
