module Atoms (
    Condition(..),
    Effect(..),
    SearchType(..),
    SearchMethod(..),
    FindCards(..),
    DestroyType(..),
) where
import GHC.Natural (Natural)
import Data.List.NonEmpty (NonEmpty)

data Condition = Destroy DestroyType FindCards
    | DiscardSelf
    | TakeDamage Natural Bool
    | HealOpponent Natural
    | Pop Natural
    | YouMay Condition
    | Choose (NonEmpty Condition)
    deriving (Eq,Ord)

data Effect = DestroyEnemy   DestroyType FindCards
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
    deriving (Eq,Ord)

monsterOnlyEffect :: Effect -> Bool
monsterOnlyEffect (Attack _) = True
monsterOnlyEffect (Attach _) = True
monsterOnlyEffect (Buff _ self) = self
monsterOnlyEffect _ = False

monsterOnlyRequirement :: Condition -> Bool
monsterOnlyRequirement  _ = False

data DestroyType = Discard | Banish deriving (Eq, Ord, Show)
data SearchType = ForName String | ForFamily String | ForSpell | ForMonster | ForCard deriving (Ord)
data FindCards = FindCardsField Natural SearchType | FindCardsHand Natural SearchType deriving (Eq,Ord)
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

