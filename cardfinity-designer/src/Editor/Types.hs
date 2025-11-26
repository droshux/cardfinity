{-# LANGUAGE OverloadedStrings #-}

module Editor.Types where

import Data.Set.Ordered qualified as OS
import GHC.Base qualified as NE
import GHC.Natural (Natural)
import Miso qualified as M
import Miso.String qualified as M
import Types qualified as C

data CardAction
  = Families (ListAction M.MisoString)
  | ToggleCardStats
  | SetImage M.MisoString
  | MAction MonsterAction
  | SAction SpellAction

data SpellAction
  = SetSpellName M.MisoString
  | SetTrigger M.MisoString
  | Effects (ListAction EffectAction)
  | CastingConditions (ListAction ConditionAction)

data MonsterAction
  = SetMonsterName M.MisoString
  | MonsterSpells (ListAction SpellAction)
  | SummoningConditions (ListAction ConditionAction)
  | SetPower Natural
  | ToggleTapped

data EffectAction
  = SetEffect EffectID
  | ESetCount Natural
  | ESetToggle1 Bool
  | ESetToggle2 Bool
  | SubEffectAction EffectAction
  | SubEffectsAction (ListAction EffectAction)
  | ESearchTypeAction SearchTypeAction
  | EConditionAction ConditionAction

data ConditionAction
  = SetCondition ConditionID
  | CSetCount Natural
  | CSetToggle1 Bool
  | CSetToggle2 Bool
  | CSearchTypeAction SearchTypeAction
  | SubConditionAction ConditionAction
  | SubConditionsAction (ListAction ConditionAction)

data SearchTypeAction = SetSearchType SearchTypeId | SetText M.MisoString

data ListAction a = NewItem | Delete Int | ItemAction Int a

newtype DeckModel = DeckModel
  { _cards :: CardModel
  }

data CardModel = CardModel
  { _spellStats :: SpellModel,
    _monsterStats :: MonsterModel,
    _families :: OS.OSet M.MisoString,
    _editingSpell :: Bool,
    _imageUrl :: M.MisoString
  }

data MonsterModel = MonsterModel
  { _monsterName :: M.MisoString,
    _monsterSpells :: [SpellModel],
    _summoningConditions :: OS.OSet ConditionModel,
    _combatPower :: Natural,
    _entersTapped :: Bool
  }

data SpellModel = SpellModel
  { _spellName :: M.MisoString,
    _spellTrigger :: C.Trigger,
    _castingConditions :: OS.OSet ConditionModel,
    _spellEffects :: [EffectModel]
  }

data ConditionModel = ConditionModel
  { _currentConditon :: ConditionID,
    _conditionCount :: Natural,
    _conditionToggle :: Bool,
    _conditionToggle2 :: Bool,
    _conditionSearchType :: SearchTypeModel,
    _subCondition :: ConditionModel,
    _subConditions :: NE.NonEmpty ConditionModel
  }

data ConditionID
  = Destroy
  | DiscardSelf
  | TakeDamage
  | HealOpponent
  | Pop
  | YouMay
  | Choose
  deriving (Enum)

data EffectModel = EffectModel
  { _currentEffect :: EffectID,
    _effectCount :: Natural,
    _effectToggle :: Bool,
    _effectToggle2 :: Bool,
    _subEffect :: EffectModel,
    _subEffects :: NE.NonEmpty EffectModel,
    _effectSearchType :: SearchTypeModel,
    _effectCondition :: ConditionModel
  }

data EffectID
  = DestroyEnemy
  | DiscardEnemy
  | DealDamage
  | Heal
  | DECKOUT
  | Draw
  | Peek
  | Scry
  | Optional
  | ChooseEffect
  | Attack
  | Play
  | Search
  | Attach
  | Buff
  | AsEffect
  deriving (Enum)

data SearchTypeModel = SearchTypeModel
  { _searchTypeID :: SearchTypeId,
    _searchTypeText :: M.MisoString
  }

data SearchTypeId
  = ForCard
  | ForMonster
  | ForSpell
  | ForName
  | ForFamily
  deriving (Enum)

instance M.ToMisoString C.Trigger where
  toMisoString C.OnPlay = "play"
  toMisoString C.OnDiscard = "discard"
  toMisoString C.OnDraw = "draw"
  toMisoString C.OnTap = "tap"
  toMisoString C.OnVictory = "victory"
  toMisoString C.OnDefeat = "defeat"
  toMisoString C.OnAttach = "attach"
  toMisoString C.Infinity = "infinity"
  toMisoString C.Counter = "counter"

instance M.FromMisoString C.Trigger where
  fromMisoStringEither "play" = Right C.OnPlay
  fromMisoStringEither "discard" = Right C.OnDiscard
  fromMisoStringEither "draw" = Right C.OnDraw
  fromMisoStringEither "tap" = Right C.OnTap
  fromMisoStringEither "victory" = Right C.OnVictory
  fromMisoStringEither "defeat" = Right C.OnDefeat
  fromMisoStringEither "attach" = Right C.OnAttach
  fromMisoStringEither "infinity" = Right C.Infinity
  fromMisoStringEither "counter" = Right C.Counter
  fromMisoStringEither s = Left ("failed to convert " ++ M.fromMisoString s ++ " to Trigger")

instance M.ToMisoString ConditionID where
  toMisoString Destroy = "Destroy"
  toMisoString DiscardSelf = "DiscardSelf"
  toMisoString TakeDamage = "TakeDamage"
  toMisoString HealOpponent = "HealOpponent"
  toMisoString Pop = "Pop"
  toMisoString YouMay = "YouMay"
  toMisoString Choose = "Choose"

instance M.FromMisoString ConditionID where
  fromMisoStringEither "Destroy" = Right Destroy
  fromMisoStringEither "DiscardSelf" = Right DiscardSelf
  fromMisoStringEither "TakeDamage" = Right TakeDamage
  fromMisoStringEither "HealOpponent" = Right HealOpponent
  fromMisoStringEither "Pop" = Right Pop
  fromMisoStringEither "YouMay" = Right YouMay
  fromMisoStringEither "Choose" = Right Choose
  fromMisoStringEither s = Left ("failed to convert " ++ M.fromMisoString s ++ " to Condition ID")

instance M.ToMisoString EffectID where
  toMisoString DestroyEnemy = "DestroyEnemy"
  toMisoString DiscardEnemy = "DiscardEnemy"
  toMisoString DealDamage = "DealDamage"
  toMisoString Heal = "Heal"
  toMisoString DECKOUT = "DECKOUT"
  toMisoString Draw = "Draw"
  toMisoString Peek = "Peek"
  toMisoString Scry = "Scry"
  toMisoString Optional = "Optional"
  toMisoString ChooseEffect = "ChooseEffect"
  toMisoString Attack = "Attack"
  toMisoString Play = "Play"
  toMisoString Search = "Search"
  toMisoString Attach = "Attach"
  toMisoString Buff = "Buff"
  toMisoString AsEffect = "AsEffect"

instance M.FromMisoString EffectID where
  fromMisoStringEither "DestroyEnemy" = Right DestroyEnemy
  fromMisoStringEither "DiscardEnemy" = Right DiscardEnemy
  fromMisoStringEither "DealDamage" = Right DealDamage
  fromMisoStringEither "Heal" = Right Heal
  fromMisoStringEither "DECKOUT" = Right DECKOUT
  fromMisoStringEither "Draw" = Right Draw
  fromMisoStringEither "Peek" = Right Peek
  fromMisoStringEither "Scry" = Right Scry
  fromMisoStringEither "Optional" = Right Optional
  fromMisoStringEither "ChooseEffect" = Right ChooseEffect
  fromMisoStringEither "Attack" = Right Attack
  fromMisoStringEither "Play" = Right Play
  fromMisoStringEither "Search" = Right Search
  fromMisoStringEither "Attach" = Right Attach
  fromMisoStringEither "Buff" = Right Buff
  fromMisoStringEither "AsEffect" = Right AsEffect
  fromMisoStringEither s = Left ("failed to convert " ++ M.fromMisoString s ++ " to Effect ID")

instance M.ToMisoString SearchTypeId where
  toMisoString ForCard = "ForCard"
  toMisoString ForMonster = "ForMonster"
  toMisoString ForSpell = "ForSpell"
  toMisoString ForName = "ForName"
  toMisoString ForFamily = "ForFamily"

instance M.FromMisoString SearchTypeId where
  fromMisoStringEither "ForCard" = Right ForCard
  fromMisoStringEither "ForMonster" = Right ForMonster
  fromMisoStringEither "ForSpell" = Right ForSpell
  fromMisoStringEither "ForName" = Right ForName
  fromMisoStringEither "ForFamily" = Right ForFamily
  fromMisoStringEither s = Left ("failed to convert " ++ M.fromMisoString s ++ " to Search Type ID")
