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

import Data.List.NonEmpty (NonEmpty ((:|)))
import GHC.Natural (Natural)
import Test.QuickCheck qualified as QC

data Condition
  = Destroy DestroyType FindCards
  | DiscardSelf
  | TakeDamage Natural Bool
  | HealOpponent Natural
  | Pop Natural
  | YouMay Condition
  | Choose (NonEmpty Condition)
  deriving (Eq, Ord)

instance QC.Arbitrary Condition where
  arbitrary =
    QC.oneof
      [ do
          dt <- QC.arbitrary
          Destroy dt <$> QC.arbitrary,
        return DiscardSelf,
        do
          n <- QC.arbitrary
          TakeDamage n <$> QC.arbitrary,
        HealOpponent <$> QC.arbitrary,
        Pop <$> QC.arbitrary,
        YouMay <$> QC.arbitrary,
        do
          front <- QC.arbitrary
          list <- QC.listOf QC.arbitrary
          return $ Choose (front :| list)
      ]

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

instance QC.Arbitrary Effect where
  arbitrary =
    QC.oneof
      [ do
          dt <- QC.arbitrary
          DestroyEnemy dt <$> QC.arbitrary,
        return DiscardEnemy,
        do
          n <- QC.arbitrary
          DealDamage n <$> QC.arbitrary,
        Heal <$> QC.arbitrary,
        return DECKOUT,
        Draw <$> QC.arbitrary,
        Peek <$> QC.arbitrary,
        Scry <$> QC.arbitrary,
        Optional <$> QC.arbitrary,
        do
          front <- QC.arbitrary
          list <- QC.listOf QC.arbitrary
          return $ ChooseEffect (front :| list),
        Attack <$> QC.arbitrary,
        Play <$> QC.arbitrary,
        Search <$> QC.arbitrary,
        Attach <$> QC.arbitrary,
        do
          i <- QC.arbitrary
          Buff i <$> QC.arbitrary,
        AsEffect <$> QC.arbitrary
      ]

monsterOnlyEffect :: Effect -> Bool
monsterOnlyEffect (Attack _) = True
monsterOnlyEffect (Attach _) = True
monsterOnlyEffect (Buff _ self) = self
monsterOnlyEffect _ = False

monsterOnlyRequirement :: Condition -> Bool
monsterOnlyRequirement _ = False

data DestroyType = Discard | Banish deriving (Eq, Ord, Show)

instance QC.Arbitrary DestroyType where
  arbitrary = QC.oneof [return Discard, return Banish]

data SearchType
  = ForName String
  | ForFamily String
  | ForSpell
  | ForMonster
  | ForCard
  deriving (Ord, Eq)

instance QC.Arbitrary SearchType where
  arbitrary =
    QC.oneof
      [ ForName <$> QC.arbitrary,
        ForFamily <$> QC.arbitrary,
        return ForSpell,
        return ForMonster,
        return ForCard
      ]

data FindCards
  = FindCardsField Natural Bool SearchType
  | FindCardsHand Natural SearchType
  deriving (Eq, Ord)

instance QC.Arbitrary FindCards where
  arbitrary = do
    n <- QC.arbitrary
    st <- QC.arbitrary
    QC.oneof
      [ (\ut -> FindCardsField n ut st) <$> QC.arbitrary,
        return $ FindCardsHand n st
      ]

data SearchMethod = SearchFor SearchType | DrillFor SearchType deriving (Eq, Ord)

instance QC.Arbitrary SearchMethod where
  arbitrary = QC.oneof $ map (<$> QC.arbitrary) [SearchFor, DrillFor]

getCount :: FindCards -> Natural
getCount (FindCardsField n _ _) = n
getCount (FindCardsHand n _) = n

getSearchType :: FindCards -> SearchType
getSearchType (FindCardsHand _ t) = t
getSearchType (FindCardsField _ _ t) = t

isField :: FindCards -> Bool
isField (FindCardsField {}) = True
isField (FindCardsHand _ _) = False
