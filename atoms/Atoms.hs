module Atoms where
import GHC.Natural (Natural)
import Utils (SearchType)
import Data.List.NonEmpty (NonEmpty)

data DestroyType = Discard | Banish deriving (Eq, Ord, Show)
data FindCards = FindCardsField Natural SearchType | FindCardsHand Natural SearchType deriving (Eq)
data SearchMethod = SearchFor SearchType | DrillFor SearchType


data Condition = Destroy DestroyType FindCards
    | Discard
    | TakeDamage Natural Bool
    | HealOpponent Natural
    | Pop Natural
    | YouMay Condition
    | Choose (NonEmpty Condition)
    deriving (Eq,Ord)

data Effect = Destroy DestroyType FindCards
    | Discard
    | DealDamage Natural Bool
    | Heal Natural
    | DECKOUT
    | Draw Natural
    | Peek Natural
    | Scry Natural
    | YouMay Effect
    | Choose (NonEmpty Effect)
    | Attack Bool
    | Play SearchType
    | Search SearchMethod
    | Attach SearchType
    | Buff Integer Bool
    | AsEffect Condition
    deriving (Eq,Ord)
