{-# LANGUAGE LambdaCase #-}

module Types
  ( Trigger (..),
    Spell (..),
    Monster (..),
    CardStats (..),
    Card (..),
    hasId,
    cardElim,
    cardElim',
    cardName,
    isMonster,
    Player (..),
    otherPlayer,
    PlayerState (..),
    CardLocation (..),
    allCardLocations,
    toLens,
    GameState (..),
    getPlayerState,
    GameOperation,
    GameOpWithCardContext,
    spellName,
    spellTrigger,
    castingConditions,
    effects,
    monsterName,
    monsterSpells,
    summoningConditions,
    combatPower,
    isTapped,
    spellStats,
    monsterStats,
    cardID,
    cardFamilies,
    cardStats,
    hand,
    field,
    deck,
    graveyard,
    autoTapList,
    isFirstTurn,
    player1State,
    player2State,
    playerLens,
    Display (..),
    isMonsterOnly,
  )
where

import Atoms (Condition, Effect)
import Control.Monad.Except
import Control.Monad.Reader (ReaderT, runReaderT)
import Control.Monad.State
import Data.Maybe (isJust)
import Data.Set.Ordered qualified as OS (OSet)
import GHC.Natural (Natural)
import Optics

data Trigger = OnPlay | OnDiscard | OnDraw | OnTap | OnVictory | OnDefeat | OnAttach | Infinity deriving (Eq, Ord)

instance Show Trigger where
  show OnPlay = "When played"
  show OnDiscard = "When discarded"
  show OnDraw = "When drawn"
  show OnTap = "Tap this card"
  show OnDefeat = "When defeated"
  show OnVictory = "When defeating a monster"
  show OnAttach = "When this spell becomes attached to a monster"
  show Infinity = "On your turn"

isMonsterOnly :: Trigger -> Bool
isMonsterOnly OnTap = True
isMonsterOnly OnDefeat = True
isMonsterOnly OnVictory = True
isMonsterOnly OnAttach = True
isMonsterOnly Infinity = True
isMonsterOnly _ = False

{- instance Default Requirement where
  def =
    Requirement
      { testRequirement = return False,
        monsterOnlyRequirement = False,
        displayRequirement = "Always"
      } -}

{- data Effect = Effect
  { performEffect :: GameOpWithCardContext (),
    monsterOnlyEffect :: Bool,
    displayEffect :: String
  }

instance Show Effect where
  show = displayEffect

instance Eq Effect where
  (==) e1 e2 = displayEffect e1 == displayEffect e2

instance Ord Effect where
  (<=) e1 e2 = displayEffect e1 <= displayEffect e2

instance Default Effect where
  def =
    Effect
      { performEffect = return (),
        monsterOnlyEffect = False,
        displayEffect = "Do nothing"
      } -}

data Spell = Spell
  { _spellName :: String,
    _spellTrigger :: Trigger,
    _castingConditions :: OS.OSet Condition,
    _effects :: [Effect]
  }
  deriving (Eq, Ord)

-- Due to a dependency loop at the core of the problem, macros cannot be used to
-- create lenses.

type SpellLens a = Lens' Spell a

spellName :: SpellLens String
spellName = lens _spellName $ \s x -> s {_spellName = x}

spellTrigger :: SpellLens Trigger
spellTrigger = lens _spellTrigger $ \s x -> s {_spellTrigger = x}

castingConditions :: SpellLens (OS.OSet Condition)
castingConditions = lens _castingConditions $ \s x -> s {_castingConditions = x}

effects :: SpellLens [Effect]
effects = lens _effects $ \s x -> s {_effects = x}

data Monster = Monster
  { _monsterName :: String,
    _monsterSpells :: [Spell],
    _summoningConditions :: OS.OSet Condition,
    _combatPower :: Natural,
    _isTapped :: Bool
  }
  deriving (Eq, Ord)

type MonsterLens a = Lens' Monster a

monsterName :: MonsterLens String
monsterName = lens _monsterName $ \m x -> m {_monsterName = x}

monsterSpells :: MonsterLens [Spell]
monsterSpells = lens _monsterSpells $ \m x -> m {_monsterSpells = x}

summoningConditions :: MonsterLens (OS.OSet Condition)
summoningConditions = lens _summoningConditions $ \m x -> m {_summoningConditions = x}

combatPower :: MonsterLens Natural
combatPower = lens _combatPower $ \m x -> m {_combatPower = x}

isTapped :: MonsterLens Bool
isTapped = lens _isTapped $ \m x -> m {_isTapped = x}

data CardStats = SpellStats Spell | MonsterStats Monster deriving (Eq, Ord)

prismSpellStats :: Prism' CardStats Spell
prismSpellStats = prism SpellStats $ \case
  SpellStats s -> Right s
  m -> Left m

prismMonsterStats :: Prism' CardStats Monster
prismMonsterStats = prism MonsterStats $ \case
  MonsterStats m -> Right m
  s -> Left s

data Card = Card
  { _cardID :: Natural,
    _cardFamilies :: OS.OSet String,
    _cardStats :: CardStats
  }

instance Eq Card where
  (==) c1 c2 = _cardFamilies c1 == _cardFamilies c2 && _cardStats c1 == _cardStats c2

instance Ord Card where
  (<=) c1 c2 = _cardStats c1 <= _cardStats c2 && _cardFamilies c1 <= _cardFamilies c2

hasId :: Natural -> Card -> Bool
hasId cid = (== cid) . _cardID

type CardLens a = Lens' Card a

cardID :: CardLens Natural
cardID = lens _cardID $ \c x -> c {_cardID = x}

cardFamilies :: CardLens (OS.OSet String)
cardFamilies = lens _cardFamilies $ \c x -> c {_cardFamilies = x}

cardStats :: CardLens CardStats
cardStats = lens _cardStats $ \c x -> c {_cardStats = x}

spellStats :: AffineTraversal' Card Spell
spellStats = cardStats % prismSpellStats

monsterStats :: AffineTraversal' Card Monster
monsterStats = cardStats % prismMonsterStats

cardElim :: (Spell -> a) -> (Monster -> a) -> Card -> a
cardElim fs fm c = case _cardStats c of
  SpellStats s -> fs s
  MonsterStats m -> fm m

cardElim' :: (Spell -> GameOpWithCardContext a) -> (Monster -> GameOpWithCardContext a) -> Card -> GameOperation a
cardElim' fs fm c = cardElim (flip runReaderT c . fs) (flip runReaderT c . fm) c

cardName :: Card -> String
cardName = cardElim _spellName _monsterName

isMonster :: Card -> Bool
isMonster = isJust . preview monsterStats

data Player = Player1 | Player2 deriving (Eq, Show)

otherPlayer :: Player -> Player
otherPlayer Player1 = Player2
otherPlayer Player2 = Player1

data PlayerState = PlayerState
  { _hand :: [Card],
    _deck :: [Card],
    -- Only monsters. cardID etc must be retained though
    _field :: [Card],
    _graveyard :: [Card],
    _autotapList :: [Natural]
  }

type PSLens = Lens' PlayerState [Card]

hand :: PSLens
hand = lens _hand $ \p x -> p {_hand = x}

deck :: PSLens
deck = lens _deck $ \p x -> p {_deck = x}

field :: PSLens
field = lens _field $ \p x -> p {_field = x}

graveyard :: PSLens
graveyard = lens _graveyard $ \p x -> p {_graveyard = x}

autoTapList :: Lens' PlayerState [Natural]
autoTapList = lens _autotapList $ \p x -> p {_autotapList = x}

data CardLocation = Hand | Deck | Field | Graveyard deriving (Eq, Show, Ord, Enum)

allCardLocations :: [CardLocation]
allCardLocations = enumFrom $ toEnum 0

toLens :: CardLocation -> Lens' PlayerState [Card]
toLens Hand = hand
toLens Deck = deck
toLens Field = field
toLens Graveyard = graveyard

data GameState = GameState
  { _player1State :: PlayerState,
    _player2State :: PlayerState,
    _isFirstTurn :: Bool
  }

player1State :: Lens' GameState PlayerState
player1State = lens _player1State $ \g p -> g {_player1State = p}

player2State :: Lens' GameState PlayerState
player2State = lens _player2State $ \g p -> g {_player2State = p}

isFirstTurn :: Lens' GameState Bool
isFirstTurn = lens _isFirstTurn $ \g b -> g {_isFirstTurn = b}

playerLens :: Player -> Lens' GameState PlayerState
playerLens = \case Player1 -> player1State; Player2 -> player2State

getPlayerState :: Player -> GameState -> PlayerState
getPlayerState Player1 = _player1State
getPlayerState Player2 = _player2State

{- Perform an operation for a given player, returning a player early if they
deckout, keeping track of the game and taking user IO -}
type GameOperation = ReaderT Player (ExceptT Player (StateT GameState IO))

type GameOpWithCardContext = ReaderT Card GameOperation

class (Show a) => Display a where
  -- Concise or not
  unparse :: Bool -> a -> String
