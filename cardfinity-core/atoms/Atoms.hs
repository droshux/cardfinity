module Atoms
  ( Condition (..),
    Effect (..),
    SearchType (..),
    SearchMethod (..),
    FindCards (..),
    DestroyType (..),
    getCount,
    getSearchType,
    isField,
    monsterOnlyEffect,
    monsterOnlyRequirement,
  )
where

import Data.List (stripPrefix)
import Data.List.NonEmpty (NonEmpty)
import GHC.Natural (Natural)
import Utils (showFold)

data Condition
  = Destroy DestroyType FindCards
  | DiscardSelf
  | TakeDamage Natural Bool
  | HealOpponent Natural
  | Pop Natural
  | YouMay Condition
  | Choose (NonEmpty Condition)
  deriving (Eq, Ord)

data Effect
  = DestroyEnemy DestroyType FindCards
  | DiscardEnemy
  | DealDamage Natural Bool
  | Heal Natural
  | DECKOUT
  | Draw Natural
  | Peek Natural
  | Scry Natural
  | Optional Effect
  | ChooseEffect (NonEmpty Effect)
  | Attack Bool
  | Play SearchType
  | Search SearchMethod
  | Attach SearchType
  | Buff Integer Bool
  | AsEffect Condition
  deriving (Eq, Ord)

monsterOnlyEffect :: Effect -> Bool
monsterOnlyEffect (Attack _) = True
monsterOnlyEffect (Attach _) = True
monsterOnlyEffect (Buff _ self) = self
monsterOnlyEffect _ = False

monsterOnlyRequirement :: Condition -> Bool
monsterOnlyRequirement _ = False

data DestroyType = Discard | Banish deriving (Eq, Ord, Show)

data SearchType = ForName String | ForFamily String | ForSpell | ForMonster | ForCard deriving (Ord)

data FindCards = FindCardsField Natural SearchType | FindCardsHand Natural SearchType deriving (Eq, Ord)

data SearchMethod = SearchFor SearchType | DrillFor SearchType deriving (Eq, Ord)

instance Eq SearchType where
  (==) (ForName _) (ForName _) = True
  (==) (ForFamily _) (ForFamily _) = True
  (==) ForSpell ForSpell = True
  (==) ForMonster ForMonster = True
  (==) ForCard ForCard = True
  (==) _ _ = False

getCount :: FindCards -> Natural
getCount (FindCardsField n _) = n
getCount (FindCardsHand n _) = n

getSearchType :: FindCards -> SearchType
getSearchType (FindCardsHand _ t) = t
getSearchType (FindCardsField _ t) = t

isField :: FindCards -> Bool
isField (FindCardsField _ _) = True
isField (FindCardsHand _ _) = False

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
      [ "See the top ",
        if n == 1 then "" else show n ++ " ",
        "card",
        if n == 1 then "" else "s",
        " of the opponent's deck"
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
