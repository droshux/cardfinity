{-# OPTIONS_GHC -Wno-orphans #-}

module AtomDisplay where

import Atoms (Condition (..), DestroyType (..), Effect (..), FindCards (..), SearchMethod (..), SearchType (..))
import GHC.Base (NonEmpty ((:|)))
import Types (Display (..))

instance Display DestroyType where
  unparse _ Discard = "discard"
  unparse _ Banish = "discard"

instance Display SearchType where
  unparse True (ForFamily f) = "f" ++ show f
  unparse False (ForFamily f) = "family " ++ show f
  unparse _ (ForName n) = show n
  unparse _ ForSpell = "spell"
  unparse _ ForMonster = "monster"
  unparse _ ForCard = "card"

instance Display FindCards where
  unparse c (FindCardsHand n t) = show n ++ " " ++ unparse c t ++ " hand"
  unparse c (FindCardsField n t) = show n ++ " " ++ unparse c t ++ " field"

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

unparseChoiceHelper :: (Display a) => Bool -> NonEmpty a -> [Char]
unparseChoiceHelper c (x1 :| []) = unparse c x1
unparseChoiceHelper c (x1 :| (x2 : rst)) = unparse c x1 ++ ", " ++ unparseChoiceHelper c (x2 :| rst)
