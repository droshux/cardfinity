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
