{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TemplateHaskell #-}

module Types
  ( Trigger (..),
    Spell (Spell),
    Monster (Monster),
    CardStats (SpellStats, MonsterStats),
    Card (Card),
    hasId,
    cardStatsElim,
    cardElim,
    cardElim',
    cardName,
    isMonster,
    Player (..),
    otherPlayer,
    autotapList,
    PlayerState,
    CardLocation (..),
    allCardLocations,
    toLens,
    GameState,
    initialGameState,
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
    cardImageUrl,
    hand,
    field,
    deck,
    graveyard,
    isFirstTurn,
    player1State,
    player2State,
    playerLens,
    Display (..),
    isMonsterOnly,
    isReaction,
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

data Trigger
  = OnPlay
  | OnDiscard
  | OnDraw
  | OnTap
  | OnVictory
  | OnDefeat
  | OnAttach
  | Infinity
  | Counter
  deriving (Eq, Ord, Enum)

instance Show Trigger where
  show OnPlay = "When played"
  show OnDiscard = "When discarded"
  show OnDraw = "When drawn"
  show OnTap = "Tap this card"
  show OnDefeat = "When defeated"
  show OnVictory = "When defeating a monster"
  show OnAttach = "When this spell becomes attached to a monster"
  show Infinity = "On your turn"
  show Counter = "Counterspell"

isMonsterOnly :: Trigger -> Bool
isMonsterOnly OnTap = True
isMonsterOnly OnDefeat = True
isMonsterOnly OnVictory = True
isMonsterOnly OnAttach = True
isMonsterOnly Infinity = True
isMonsterOnly _ = False

isReaction :: Trigger -> Bool
isReaction OnPlay = True
isReaction Counter = True
isReaction _ = False

data Spell = Spell
  { _spellName :: String,
    _spellTrigger :: Trigger,
    _castingConditions :: OS.OSet Condition,
    _effects :: [Effect]
  }
  deriving (Eq, Ord)

$(makeLenses ''Spell)

data Monster = Monster
  { _monsterName :: String,
    _monsterSpells :: [Spell],
    _summoningConditions :: OS.OSet Condition,
    _combatPower :: Natural,
    _isTapped :: Bool
  }
  deriving (Eq, Ord)

$(makeLenses ''Monster)

data CardStats = SpellStats Spell | MonsterStats Monster deriving (Eq, Ord)

$(makePrisms ''CardStats)

cardStatsElim :: (Spell -> a) -> (Monster -> a) -> CardStats -> a
cardStatsElim f _ (SpellStats s) = f s
cardStatsElim _ g (MonsterStats m) = g m

data Card = Card
  { _cardID :: Natural,
    _cardFamilies :: OS.OSet String,
    _cardStats :: CardStats,
    _cardImageUrl :: Maybe String
  }

$(makeLenses ''Card)

instance Eq Card where
  (==) c1 c2 = _cardFamilies c1 == _cardFamilies c2 && _cardStats c1 == _cardStats c2

instance Ord Card where
  (<=) c1 c2 = _cardStats c1 <= _cardStats c2 && _cardFamilies c1 <= _cardFamilies c2

hasId :: Natural -> Card -> Bool
hasId cid = (== cid) . _cardID

spellStats :: AffineTraversal' Card Spell
spellStats = cardStats % _SpellStats

monsterStats :: AffineTraversal' Card Monster
monsterStats = cardStats % _MonsterStats

cardElim :: (Spell -> a) -> (Monster -> a) -> Card -> a
cardElim f g = cardStatsElim f g . _cardStats

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

$(makeLenses ''PlayerState)

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

$(makeLenses ''GameState)

initialGameState :: ([Card], [Card]) -> ([Card], [Card]) -> GameState
initialGameState (hand1, deck1) (hand2, deck2) =
  GameState
    { _player1State =
        PlayerState
          { _hand = hand1,
            _graveyard = [],
            _field = [],
            _deck = deck1,
            _autotapList = []
          },
      _player2State =
        PlayerState
          { _hand = hand2,
            _graveyard = [],
            _field = [],
            _deck = deck2,
            _autotapList = []
          },
      _isFirstTurn = True
    }

playerLens :: Player -> Lens' GameState PlayerState
playerLens = \case Player1 -> player1State; Player2 -> player2State

{- Perform an operation for a given player, returning a player early if they
deckout, keeping track of the game and taking user IO -}
type GameOperation = ReaderT Player (ExceptT Player (StateT GameState IO))

type GameOpWithCardContext = ReaderT Card GameOperation

cardElim' :: (Spell -> GameOpWithCardContext a) -> (Monster -> GameOpWithCardContext a) -> Card -> GameOperation a
cardElim' fs fm c = cardElim (flip runReaderT c . fs) (flip runReaderT c . fm) c

class (Show a) => Display a where
  -- Concise or not
  unparse :: Bool -> a -> String
