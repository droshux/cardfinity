{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Editor.Types
  ( TriggerID (..),
    SearchTypeID (..),
    ConditionID (..),
    EffectID (..),
    DeckAction (..),
    CardAction (..),
    SpellAction (..),
    MonsterAction (..),
    EffectAction (..),
    ConditionAction (..),
    SearchTypeAction (..),
    ListAction (..),
    SearchTypeModel,
    searchTypeID,
    searchTypeText,
    ConditionModel,
    currentCondition,
    currentCondition',
    conditionCount,
    conditionToggle,
    conditionToggle2,
    conditionToggle3,
    conditionSearchType,
    conditionOptional,
    subConditions,
    currentEffect,
    currentEffect',
    EffectModel,
    effectCount,
    effectCountInt,
    effectToggle,
    effectToggle2,
    effectToggle3,
    effectOptional,
    subEffects,
    effectSearchType,
    effectCondition,
    SpellModel,
    spellName,
    spellTrigger,
    castingConditions,
    spellEffects,
    MonsterModel,
    monsterName,
    monsterSpells,
    summoningConditions,
    combatPower,
    entersTapped,
    CardModel,
    spellStats,
    monsterStats,
    families,
    editingSpell,
    imageUrl,
    DeckModel,
    deck,
    currentCardIndex,
    showDecklist,
    author,
    deckName,
    Default (..),
    applyOptionalCond,
    applyOptionalEff,
    IsTrigger (..),
  )
where

import GHC.Generics (Generic)
import GHC.Natural (Natural, naturalFromInteger)
import GHC.Num (integerFromInt)
import Miso qualified as M
import Miso.JSON (FromJSON, ToJSON)
import Miso.Lens (Lens, lens)
import Miso.Lens.TH (makeLenses)
import Miso.String (FromMisoString (fromMisoStringEither))
import Miso.String qualified as M
import Test.QuickCheck qualified as QC
import Types qualified as CF

data TriggerID
  = OnPlay
  | OnDiscard
  | OnDraw
  | OnTap
  | OnVictory
  | OnDefeat
  | OnAttach
  | Infinity
  | Counter
  deriving (Eq, Enum, Bounded, Show, Generic, ToJSON, FromJSON)

instance QC.Arbitrary TriggerID where
  arbitrary = QC.chooseEnum (minBound, maxBound)

class IsTrigger a where
  toTrigger :: a -> CF.Trigger
  fromTrigger :: CF.Trigger -> a

instance IsTrigger TriggerID where
  toTrigger = toEnum . fromEnum
  fromTrigger = toEnum . fromEnum

instance IsTrigger CF.Trigger where
  toTrigger = id
  fromTrigger = id

data SearchTypeID
  = ForCard
  | ForMonster
  | ForSpell
  | ForName
  | ForFamily
  deriving (Enum, Bounded, Eq, Ord, Generic, FromJSON, ToJSON)

instance QC.Arbitrary SearchTypeID where
  arbitrary = QC.chooseEnum (minBound, maxBound)

data ConditionID
  = DiscardSelf -- This is first because it needs no other inputs
  | Destroy
  | TakeDamage
  | HealOpponent
  | Pop
  | YouMay
  | Choose
  deriving (Enum, Bounded, Eq, Ord, Generic, FromJSON, ToJSON)

instance QC.Arbitrary ConditionID where
  arbitrary = QC.chooseEnum (minBound, maxBound)

data EffectID
  = DiscardEnemy
  | DestroyEnemy
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
  deriving (Enum, Bounded, Eq, Generic, FromJSON, ToJSON)

instance QC.Arbitrary EffectID where
  arbitrary = QC.chooseEnum (minBound, maxBound)

data DeckAction
  = NewCard
  | SetCopies Int Int
  | ViewCard Int
  | DeleteCard Int
  | ToggleDecklist
  | ActCard Int CardAction
  | MoveUp
  | MoveDown
  | SetDeckName M.MisoString
  | SetAuthor M.MisoString
  | UpdateParent

data CardAction
  = ToggleCardStats
  | SetImage M.MisoString
  | ActSpell SpellAction
  | ActMonster MonsterAction
  | ActFamilies (ListAction M.MisoString)

data SpellAction
  = SetSpellName M.MisoString
  | SetTrigger TriggerID
  | ActCond (ListAction ConditionAction)
  | ActEff (ListAction EffectAction)

data MonsterAction
  = SetMonsterName M.MisoString
  | SetPower Natural
  | ToggleTapped
  | ActSummonCond (ListAction ConditionAction)
  | ActSpells (ListAction SpellAction)

data EffectAction
  = SetEffect EffectID
  | ESetCount Natural
  | ESetCountInt Integer
  | EToggle1
  | EToggle2
  | EToggle3
  | EffSearchType SearchTypeAction
  | ESetOptional EffectID
  | SubEffsAction (ListAction EffectAction)
  | EffCondAction ConditionAction

data ConditionAction
  = SetCondition ConditionID
  | CSetCount Natural
  | CToggle1
  | CToggle2
  | CToggle3
  | CondSearchType SearchTypeAction
  | CSetOptional ConditionID
  | SubCondsAction (ListAction ConditionAction)

data SearchTypeAction = SetSearchType SearchTypeID | SetText M.MisoString

data ListAction a = NewItem | Delete Int | Act Int a

instance Show SearchTypeID where
  show ForCard = "Card"
  show ForMonster = "Monster"
  show ForSpell = "Spell"
  show ForName = "By Name"
  show ForFamily = "By Family"

instance Show ConditionID where
  show Destroy = "Discard/Banish"
  show DiscardSelf = "Discard Top"
  show TakeDamage = "Take damage"
  show HealOpponent = "Heal the opponent"
  show Pop = "Pop"
  show YouMay = "Optional"
  show Choose = "Choose"

instance Show EffectID where
  show DestroyEnemy = "Discard/Banish Enemy"
  show DiscardEnemy = "Discard Enemy Top"
  show DealDamage = "Deal Damage"
  show Heal = "Heal"
  show DECKOUT = "DECKOUT"
  show Draw = "Draw"
  show Peek = "Peek"
  show Scry = "Scry"
  show Optional = "You May"
  show ChooseEffect = "Choose"
  show Attack = "Attack"
  show Play = "Play"
  show Search = "Search/Drill"
  show Attach = "Attach"
  show Buff = "Buff"
  show AsEffect = "As Effect"

instance QC.Arbitrary M.MisoString where
  arbitrary = M.toMisoString <$> (QC.arbitrary :: QC.Gen String)

data SearchTypeModel = SearchTypeModel
  { _searchTypeID :: SearchTypeID,
    _searchTypeText :: M.MisoString
  }
  deriving (Eq, Ord, Generic, FromJSON, ToJSON)

$(makeLenses ''SearchTypeModel)

instance QC.Arbitrary SearchTypeModel where
  arbitrary = do
    stId <- QC.arbitrary
    txt <- QC.arbitrary
    return $ SearchTypeModel {_searchTypeID = stId, _searchTypeText = txt}

data ConditionModel = ConditionModel
  { _currentCondition :: ConditionID,
    _conditionCount :: Natural,
    _conditionToggle :: Bool,
    _conditionToggle2 :: Bool,
    _conditionToggle3 :: Bool,
    _conditionSearchType :: SearchTypeModel,
    _conditionOptional :: ConditionID,
    _subConditions :: [ConditionModel]
  }
  deriving (Eq, Ord, Generic, FromJSON, ToJSON)

applyOptionalCond :: ConditionModel -> ConditionModel
applyOptionalCond m = m {_currentCondition = _conditionOptional m}

currentCondition' :: Lens ConditionModel ConditionID
currentCondition' =
  let get m =
        if _currentCondition m == YouMay
          then _conditionOptional m
          else _currentCondition m
      set m condId =
        if _currentCondition m == YouMay
          then m {_conditionOptional = condId}
          else m {_currentCondition = condId}
   in lens get set

$(makeLenses ''ConditionModel)

instance QC.Arbitrary ConditionModel where
  arbitrary =
    ConditionModel
      <$> QC.arbitrary
      <*> QC.arbitrary
      <*> QC.arbitrary
      <*> QC.arbitrary
      <*> QC.arbitrary
      <*> QC.arbitrary
      <*> QC.arbitrary
      <*> QC.listOf QC.arbitrary

data EffectModel = EffectModel
  { _currentEffect :: EffectID,
    _effectCount :: Natural,
    _effectCountInt :: Integer,
    _effectToggle :: Bool,
    _effectToggle2 :: Bool,
    _effectToggle3 :: Bool,
    _effectOptional :: EffectID,
    _subEffects :: [EffectModel],
    _effectSearchType :: SearchTypeModel,
    _effectCondition :: ConditionModel
  }
  deriving (Eq, Generic, FromJSON, ToJSON)

applyOptionalEff :: EffectModel -> EffectModel
applyOptionalEff m = m {_currentEffect = _effectOptional m}

currentEffect' :: Lens EffectModel EffectID
currentEffect' =
  let get m =
        if _currentEffect m == Optional
          then _effectOptional m
          else _currentEffect m
      set m effId =
        if _currentEffect m == Optional
          then m {_effectOptional = effId}
          else m {_currentEffect = effId}
   in lens get set

$(makeLenses ''EffectModel)

instance QC.Arbitrary EffectModel where
  arbitrary =
    EffectModel
      <$> QC.arbitrary
      <*> QC.arbitrary
      <*> QC.arbitrary
      <*> QC.arbitrary
      <*> QC.arbitrary
      <*> QC.arbitrary
      <*> QC.arbitrary
      <*> QC.listOf QC.arbitrary
      <*> QC.arbitrary
      <*> QC.arbitrary

data SpellModel = SpellModel
  { _spellName :: M.MisoString,
    _spellTrigger :: TriggerID,
    _castingConditions :: [ConditionModel],
    _spellEffects :: [EffectModel]
  }
  deriving (Eq, Generic, FromJSON, ToJSON)

$(makeLenses ''SpellModel)

instance QC.Arbitrary SpellModel where
  arbitrary =
    SpellModel
      <$> QC.arbitrary
      <*> QC.arbitrary
      <*> QC.listOf QC.arbitrary
      <*> QC.listOf QC.arbitrary

data MonsterModel = MonsterModel
  { _monsterName :: M.MisoString,
    _monsterSpells :: [SpellModel],
    _summoningConditions :: [ConditionModel],
    _combatPower :: Natural,
    _entersTapped :: Bool
  }
  deriving (Eq, Generic, FromJSON, ToJSON)

$(makeLenses ''MonsterModel)

instance QC.Arbitrary MonsterModel where
  arbitrary =
    MonsterModel
      <$> QC.arbitrary
      <*> QC.listOf QC.arbitrary
      <*> QC.listOf QC.arbitrary
      <*> QC.arbitrary
      <*> QC.arbitrary

data CardModel = CardModel
  { _spellStats :: SpellModel,
    _monsterStats :: MonsterModel,
    _families :: [M.MisoString],
    _editingSpell :: Bool,
    _imageUrl :: M.MisoString
  }
  deriving (Eq, Generic, FromJSON, ToJSON)

$(makeLenses ''CardModel)

instance QC.Arbitrary CardModel where
  arbitrary =
    CardModel
      <$> QC.arbitrary
      <*> QC.arbitrary
      <*> QC.listOf QC.arbitrary
      <*> QC.arbitrary
      <*> QC.arbitrary

data DeckModel = DeckModel
  { _deck :: [(Int, CardModel)],
    _currentCardIndex :: Int,
    _showDecklist :: Bool,
    _author :: M.MisoString,
    _deckName :: M.MisoString
  }
  deriving (Eq, Generic, FromJSON, ToJSON)

$(makeLenses ''DeckModel)

instance QC.Arbitrary DeckModel where
  arbitrary =
    DeckModel
      <$> QC.listOf (QC.liftArbitrary2 QC.arbitrary QC.arbitrary)
      <*> QC.arbitrary
      <*> QC.arbitrary
      <*> QC.arbitrary
      <*> QC.arbitrary

class Default a where
  def :: a

instance Default DeckModel where
  def =
    DeckModel
      { _deck = [(1, def)],
        _currentCardIndex = 0,
        _showDecklist = True,
        _author = "",
        _deckName = "New deck"
      }

instance Default CardModel where
  def =
    CardModel
      { _spellStats = def,
        _monsterStats = def,
        _families = [],
        _editingSpell = True,
        _imageUrl = def
      }

instance Default (Int, CardModel) where
  def = (1, def)

instance Default MonsterModel where
  def =
    MonsterModel
      { _monsterName = def,
        _monsterSpells = [],
        _summoningConditions = [],
        _combatPower = 0,
        _entersTapped = False
      }

instance Default SpellModel where
  def =
    SpellModel
      { _spellName = def,
        _spellTrigger = OnPlay,
        _castingConditions = [],
        _spellEffects = []
      }

instance Default SearchTypeModel where
  def =
    SearchTypeModel
      { _searchTypeID = ForCard,
        _searchTypeText = def
      }

instance Default ConditionModel where
  def =
    ConditionModel
      { _currentCondition = DiscardSelf,
        _conditionCount = 0,
        _conditionToggle = False,
        _conditionToggle2 = False,
        _conditionToggle3 = True,
        _conditionSearchType = def,
        _conditionOptional = DiscardSelf,
        _subConditions = []
      }

instance Default EffectModel where
  def =
    EffectModel
      { _currentEffect = DiscardEnemy,
        _effectCount = 0,
        _effectCountInt = 0,
        _effectToggle = False,
        _effectToggle2 = False,
        _effectToggle3 = False,
        _effectOptional = DiscardEnemy,
        _subEffects = [],
        _effectSearchType = def,
        _effectCondition = def
      }

instance Default M.MisoString where
  def = ""

instance M.ToMisoString TriggerID where
  toMisoString OnPlay = "play"
  toMisoString OnDiscard = "discard"
  toMisoString OnDraw = "draw"
  toMisoString OnTap = "tap"
  toMisoString OnVictory = "victory"
  toMisoString OnDefeat = "defeat"
  toMisoString OnAttach = "attach"
  toMisoString Infinity = "infinity"
  toMisoString Counter = "counter"

instance M.FromMisoString TriggerID where
  fromMisoStringEither "play" = Right OnPlay
  fromMisoStringEither "discard" = Right OnDiscard
  fromMisoStringEither "draw" = Right OnDraw
  fromMisoStringEither "tap" = Right OnTap
  fromMisoStringEither "victory" = Right OnVictory
  fromMisoStringEither "defeat" = Right OnDefeat
  fromMisoStringEither "attach" = Right OnAttach
  fromMisoStringEither "infinity" = Right Infinity
  fromMisoStringEither "counter" = Right Counter
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

instance M.ToMisoString SearchTypeID where
  toMisoString ForCard = "ForCard"
  toMisoString ForMonster = "ForMonster"
  toMisoString ForSpell = "ForSpell"
  toMisoString ForName = "ForName"
  toMisoString ForFamily = "ForFamily"

instance M.FromMisoString SearchTypeID where
  fromMisoStringEither "ForCard" = Right ForCard
  fromMisoStringEither "ForMonster" = Right ForMonster
  fromMisoStringEither "ForSpell" = Right ForSpell
  fromMisoStringEither "ForName" = Right ForName
  fromMisoStringEither "ForFamily" = Right ForFamily
  fromMisoStringEither s = Left ("failed to convert " ++ M.fromMisoString s ++ " to Search Type ID")

instance M.FromMisoString Natural where
  fromMisoStringEither = fmap (naturalFromInteger . integerFromInt) . fromMisoStringEither

instance M.FromMisoString Integer where
  fromMisoStringEither = fmap integerFromInt . fromMisoStringEither
