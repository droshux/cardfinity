{-# OPTIONS_GHC -Wno-orphans #-}

module AtomDisplay where

import Atoms (Condition (..), DestroyType (..), Effect (..), FindCards (..), SearchMethod (..), SearchType (..))
import Data.List.NonEmpty (NonEmpty ((:|)))
import Data.List.NonEmpty qualified as NE
import GHC.Natural (Natural)
import Types (CardText (..), Display (..), firstText, num, txt)

instance Display DestroyType where
  unparse _ Discard = "discard"
  unparse _ Banish = "banish"
  show' Discard = Keyword "Discard"
  show' Banish = Keyword "Banish"

instance Display SearchType where
  unparse True (ForFamily f) = "f" ++ show f
  unparse False (ForFamily f) = "family " ++ show f
  unparse _ (ForName n) = show n
  unparse _ ForSpell = "spell"
  unparse _ ForMonster = "monster"
  unparse _ ForCard = "card"
  show' (ForFamily f) = CardFamily f <> txt " card"
  show' (ForName n) = CardName n
  show' ForSpell = Keyword "Spell"
  show' ForMonster = Keyword "Monster"
  show' ForCard = Keyword "Card"

instance Show SearchType where
  show = show . show'

instance Display FindCards where
  unparse c (FindCardsHand n t) = show n ++ " " ++ unparse c t ++ " hand"
  unparse c (FindCardsField n t) = show n ++ " " ++ unparse c t ++ " field"
  show' = findCardsShow' False

instance Show FindCards where
  show = show . show'

findCardsShow' :: Bool -> FindCards -> CardText
findCardsShow' enemy f = case f of
  FindCardsHand n t -> part1 n t <> Keyword "Hand"
  FindCardsField n t -> part1 n t <> Keyword "Field"
  where
    part1 n t =
      mconcat
        [ num n,
          txt " ",
          show' t,
          if n == 1 then mempty else txt "s",
          txt " from the ",
          if enemy then Keyword "enemy" <> txt " enemy" else mempty
        ]

instance Display Condition where
  unparse c (Destroy d f) = unparse c d ++ " " ++ unparse c f
  unparse _ DiscardSelf = "discard"
  unparse True (TakeDamage n True) = "take " ++ show n ++ "t"
  unparse False (TakeDamage n True) = "take " ++ show n ++ " true"
  unparse _ (TakeDamage n False) = "take " ++ show n
  unparse _ (HealOpponent n) = "heal enemy " ++ show n
  unparse _ (Pop n) = "pop " ++ show n
  unparse c (YouMay cond) = unparse c cond ++ "?"
  unparse c (Choose cs) = "(" ++ unparseChoiceHelper c cs ++ ")"
  show' (Destroy d f) = show' d <> txt " " <> show' f
  show' DiscardSelf = Keyword "Discard" <> txt " the top card of the deck"
  show' (TakeDamage n isTrue) = mconcat [Keyword "Take ", num n, if isTrue then txt " " <> Keyword "True" else mempty, txt " damage"]
  show' (HealOpponent n) = Keyword "Heal" <> txt " the opponent for " <> num n <> txt " damage"
  show' (Pop n) =
    mconcat
      [ Keyword "Banish",
        txt " the top ",
        if n == 1 then txt "card" else num n <> txt " cards",
        txt " of the ",
        Keyword "Graveyard"
      ]
  show' (YouMay cond) = txt "You " <> Keyword "can" <> txt " " <> show' cond
  show' (Choose cs) = Keyword "Choose" <> txt " one of (" <> List (NE.map show' cs) <> txt ")"

instance Show Condition where
  show = show . show'

instance Display Effect where
  unparse c (DestroyEnemy d _) = unparse c d ++ " enemy " ++ unparse c d
  unparse _ DiscardEnemy = "discard enemy"
  unparse True (DealDamage n True) = "deal " ++ show n ++ "t"
  unparse False (DealDamage n True) = "deal " ++ show n ++ " true"
  unparse _ (DealDamage n False) = "deal " ++ show n
  unparse _ (Heal n) = "heal " ++ show n
  unparse _ DECKOUT = "deckout"
  unparse _ (Draw n) = "draw " ++ show n
  unparse _ (Peek n) = "peek " ++ show n
  unparse _ (Scry n) = "scry " ++ show n
  unparse c (Optional e) = unparse c e ++ "?"
  unparse c (ChooseEffect es) = "(" ++ unparseChoiceHelper c es ++ ")"
  unparse _ (Attack piercing) = (if piercing then "piercing " else "") ++ "attack"
  unparse c (Play t) = "play " ++ unparse c t
  unparse c (Search (SearchFor t)) = "search " ++ unparse c t
  unparse c (Search (DrillFor t)) = "drill " ++ unparse c t
  unparse c (Attach t) = "attach " ++ unparse c t
  unparse _ (Buff n forItself) = "buff " ++ (if forItself then "this " else "") ++ show n
  unparse c (AsEffect cond) = unparse c cond
  show' (DestroyEnemy d f) = show' d <> txt " " <> findCardsShow' True f
  show' DiscardEnemy = Keyword "Discard" <> txt " the top card of the enemy deck"
  show' (DealDamage n isTrue) = mconcat [txt "Deal ", num n, txt " ", if isTrue then Keyword "True" else mempty, txt " damage"]
  show' (Heal n) = Keyword "Heal" <> txt " " <> num n <> txt " damage"
  show' DECKOUT = Keyword "DECKOUT"
  show' (Draw n) =
    mconcat
      [ Keyword "Draw",
        txt " ",
        if n == 1 then txt "a" else num n,
        txt " card",
        if n == 1 then mempty else txt "s"
      ]
  show' (Peek n) = peekScryShow' "Peek" False n
  show' (Scry n) = peekScryShow' "Peek" True n
  show' (Optional e) = txt "You " <> Keyword "may" <> txt " " <> show' e
  show' (ChooseEffect es) =
    Keyword "Choose"
      <> txt " one of ("
      <> List (NE.map show' es)
      <> txt ")"
  show' (Attack piercing) =
    Keyword "Attack"
      <> txt " with this monster"
      <> if piercing then txt " dealing " <> Keyword "piercing" <> txt " damage" else mempty
  show' (Play t) = let st' = show' t in Keyword "Play" <> aOrAn st' <> st'
  show' (Search (SearchFor t)) =
    let st' = show' t
     in Keyword "Search" <> txt " the deck for" <> aOrAn st' <> st'
  show' (Search (DrillFor t)) =
    let st' = show' t
     in Keyword "Drill" <> txt " the deck for" <> aOrAn st' <> st'
  show' (Attach t) =
    let st' = show' t
     in Keyword "Attach" <> aOrAn st' <> st' <> txt " from your hand to this card"
  show' (Buff by forItself) =
    let give = if forItself then mempty else Keyword "Give" <> txt " "
     in give <> num by <> txt " power"
  show' (AsEffect c) = show' c

instance Show Effect where
  show = show . show'

aOrAn :: CardText -> CardText
aOrAn t =
  let vowels = ['A', 'a', 'E', 'e', 'I', 'i', 'O', 'o', 'U', 'u']
   in case firstText t of
        [] -> mempty
        (c : _) -> if c `elem` vowels then txt " an " else txt " a "

peekScryShow' :: String -> Bool -> Natural -> CardText
peekScryShow' s enemy n =
  mconcat
    [ Keyword s,
      txt " at the top ",
      if n == 1 then mempty else num n <> txt " ",
      txt "card",
      if n == 1 then mempty else txt "s",
      txt " of the",
      if enemy then Keyword " enemy" else mempty,
      txt " deck"
    ]

unparseChoiceHelper :: (Display a) => Bool -> NonEmpty a -> [Char]
unparseChoiceHelper c (x1 :| []) = unparse c x1
unparseChoiceHelper c (x1 :| (x2 : rst)) = unparse c x1 ++ ", " ++ unparseChoiceHelper c (x2 :| rst)
