{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Editor.Types
  ( SearchTypeID (..),
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
    conditionCount,
    conditionToggle,
    conditionToggle2,
    conditionSearchType,
    subCondition,
    subConditions,
    currentEffect,
    EffectModel,
    effectCount,
    effectCountInt,
    effectToggle,
    effectToggle2,
    subEffect,
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
    Default (..),
  )
where

import Data.List.NonEmpty (NonEmpty ((:|)))
import Data.Set.Ordered (OSet, empty)
import GHC.Natural (Natural, naturalFromInteger)
import GHC.Num (integerFromInt)
import Miso qualified as M
import Miso.Lens (Lens, lens, (^.))
import Miso.Lens.TH (makeLenses)
import Miso.String (FromMisoString (fromMisoStringEither))
import Miso.String qualified as M
import Types (Trigger (..))

data SearchTypeID
  = ForCard
  | ForMonster
  | ForSpell
  | ForName
  | ForFamily
  deriving (Enum, Eq, Ord)

data ConditionID
  = DiscardSelf -- This is first because it needs no other inputs
  | Destroy
  | TakeDamage
  | HealOpponent
  | Pop
  | YouMay
  | Choose
  deriving (Enum, Eq, Ord)

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
  deriving (Enum, Eq)

data DeckAction
  = NewCard
  | SetCopies Int Natural
  | DCardAction Int CardAction
  | ViewCard Int
  | DeleteCard Int
  | ToggleDecklist

data CardAction
  = Families (ListAction M.MisoString)
  | ToggleCardStats
  | SetImage M.MisoString
  | MAction MonsterAction
  | SAction SpellAction

data SpellAction
  = SetSpellName M.MisoString
  | SetTrigger Trigger
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
  | SetCountInt Integer
  | EToggle1
  | EToggle2
  | SubEffectAction EffectAction
  | SubEffectsAction (ListAction EffectAction)
  | ESearchTypeAction SearchTypeAction
  | EConditionAction ConditionAction

data ConditionAction
  = SetCondition ConditionID
  | CSetCount Natural
  | CToggle1
  | CToggle2
  | CSearchTypeAction SearchTypeAction
  | SubConditionAction ConditionAction
  | SubConditionsAction (ListAction ConditionAction)

data SearchTypeAction = SetSearchType SearchTypeID | SetText M.MisoString

data ListAction a = NewItem | Delete Int | ItemAction Int a

data SearchTypeModel = SearchTypeModel
  { _searchTypeID :: SearchTypeID,
    _searchTypeText :: M.MisoString
  }
  deriving (Eq, Ord)

$(makeLenses ''SearchTypeModel)

data ConditionModel = ConditionModel
  { _currentCondition :: ConditionID,
    _conditionCount :: Natural,
    _conditionToggle :: Bool,
    _conditionToggle2 :: Bool,
    _conditionSearchType :: SearchTypeModel,
    _subCondition :: Maybe ConditionModel,
    _subConditions :: Maybe (NonEmpty ConditionModel)
  }
  deriving (Eq, Ord)

$(makeLenses ''ConditionModel)

data EffectModel = EffectModel
  { _currentEffect :: EffectID,
    _effectCount :: Natural,
    _effectCountInt :: Integer,
    _effectToggle :: Bool,
    _effectToggle2 :: Bool,
    _subEffect :: Maybe EffectModel,
    _subEffects :: Maybe (NonEmpty EffectModel),
    _effectSearchType :: SearchTypeModel,
    _effectCondition :: ConditionModel
  }
  deriving (Eq)

$(makeLenses ''EffectModel)

data SpellModel = SpellModel
  { _spellName :: M.MisoString,
    _spellTrigger :: Trigger,
    _castingConditions :: OSet ConditionModel,
    _spellEffects :: [EffectModel]
  }
  deriving (Eq)

$(makeLenses ''SpellModel)

data MonsterModel = MonsterModel
  { _monsterName :: M.MisoString,
    _monsterSpells :: [SpellModel],
    _summoningConditions :: OSet ConditionModel,
    _combatPower :: Natural,
    _entersTapped :: Bool
  }
  deriving (Eq)

$(makeLenses ''MonsterModel)

data CardModel = CardModel
  { _spellStats :: SpellModel,
    _monsterStats :: MonsterModel,
    _families :: OSet M.MisoString,
    _editingSpell :: Bool,
    _imageUrl :: M.MisoString
  }
  deriving (Eq)

$(makeLenses ''CardModel)

data DeckModel = DeckModel
  { _deck :: [(Int, CardModel)],
    _currentCardIndex :: Int,
    _showDecklist :: Bool
  }
  deriving (Eq)

$(makeLenses ''DeckModel)

class Default a where
  def :: a

instance Default DeckModel where
  def =
    DeckModel
      { _deck = [(0, def)],
        _currentCardIndex = 0,
        _showDecklist = True
      }

instance Default CardModel where
  def =
    CardModel
      { _spellStats = def,
        _monsterStats = def,
        _families = empty,
        _editingSpell = True,
        _imageUrl = ""
      }

instance Default MonsterModel where
  def =
    MonsterModel
      { _monsterName = "",
        _monsterSpells = [],
        _summoningConditions = empty,
        _combatPower = 0,
        _entersTapped = False
      }

instance Default SpellModel where
  def =
    SpellModel
      { _spellName = "",
        _spellTrigger = OnPlay,
        _castingConditions = empty,
        _spellEffects = []
      }

instance Default SearchTypeModel where
  def =
    SearchTypeModel
      { _searchTypeID = ForCard,
        _searchTypeText = ""
      }

instance Default ConditionModel where
  def =
    ConditionModel
      { _currentCondition = DiscardSelf,
        _conditionCount = 0,
        _conditionToggle = False,
        _conditionToggle2 = False,
        _conditionSearchType = def,
        _subCondition = Nothing,
        _subConditions = Nothing
      }

instance Default EffectModel where
  def =
    EffectModel
      { _currentEffect = DiscardEnemy,
        _effectCount = 0,
        _effectCountInt = 0,
        _effectToggle = False,
        _effectToggle2 = False,
        _subEffect = Nothing,
        _subEffects = Nothing,
        _effectSearchType = def,
        _effectCondition = def
      }

instance Default M.MisoString where
  def = ""

instance (Default a) => Default (NonEmpty a) where
  def = def :| []

instance M.ToMisoString Trigger where
  toMisoString OnPlay = "play"
  toMisoString OnDiscard = "discard"
  toMisoString OnDraw = "draw"
  toMisoString OnTap = "tap"
  toMisoString OnVictory = "victory"
  toMisoString OnDefeat = "defeat"
  toMisoString OnAttach = "attach"
  toMisoString Infinity = "infinity"
  toMisoString Counter = "counter"

instance M.FromMisoString Trigger where
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
