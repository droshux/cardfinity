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

import Data.List.NonEmpty (NonEmpty)
import GHC.Natural (Natural)

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

data SearchType = ForName String | ForFamily String | ForSpell | ForMonster | ForCard deriving (Ord, Eq)

data FindCards = FindCardsField Natural Bool SearchType | FindCardsHand Natural SearchType deriving (Eq, Ord)

data SearchMethod = SearchFor SearchType | DrillFor SearchType deriving (Eq, Ord)

getCount :: FindCards -> Natural
getCount (FindCardsField n _ _) = n
getCount (FindCardsHand n _) = n

getSearchType :: FindCards -> SearchType
getSearchType (FindCardsHand _ t) = t
getSearchType (FindCardsField _ _ t) = t

isField :: FindCards -> Bool
isField (FindCardsField {}) = True
isField (FindCardsHand _ _) = False
