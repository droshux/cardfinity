{-# LANGUAGE LambdaCase #-}

module Types
  ( isLegalDeck,
    Ex (..),
    Scale,
    HasScale (..),
    Trigger (..),
    isMonsterOnly,
    Requirement (..),
    (~>),
    reqs,
    Effect (..),
    Spell (..),
    Monster (..),
    CardStats (..),
    Card (..),
    cardElim,
    cardElim',
    cardName,
    isMonster,
    Player (..),
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
    isFirstTurn,
    player1State,
    player2State,
    playerLens,
  )
where

import Control.Monad.Except
import Control.Monad.Reader (ReaderT (runReaderT))
import Control.Monad.State
import Data.Data (Typeable, cast)
import Data.Foldable (Foldable (toList))
import Data.Kind (Constraint, Type)
import Data.Maybe (isJust)
import Data.Set.Ordered qualified as OS (OSet, singleton, (|>))
import GHC.Natural (Natural)
import Optics

--------------------------------------------------------------------------------
-- CARD BALANCING AREA:
punishment :: Scale
punishment = 10

instance HasScale Trigger where
  scale Infinity = 50
  scale OnPlay = 0
  scale _ = 5

instance HasScale Spell where
  scale (Spell _ t c e) =
    scale t
      + sum (map scale $ toList c)
      + sum (map scale e)
      + punishment * max 0 (length e - 1)

instance HasScale Monster where
  scale (Monster _ ss c p t) =
    sum (map scale $ toList c)
      + sum (map (max 0 . scale) ss)
      + punishment * max 0 (length ss - 1)
      + sp p
      + if anyTap ss && t then -5 else 0 -- Enters the field tapped
    where
      anyTap = any ((OnTap ==) . (^. spellTrigger))
      sp v =
        let f :: Float = fromIntegral v
            sc :: Scale = fromIntegral v
         in sc * ceiling (logBase 10.0 f)

isLegal :: CardStats -> Bool
isLegal (SpellStats s) =
  -- MonsterOnly effects cannot appear on spells that could be played as cards
  scale s <= 10 && (mot || (not moe && not mor))
  where
    moe = any monsterOnlyEffect $ _effects s
    mor = any monsterOnlyRequirement $ _castingConditions s
    mot = isMonsterOnly $ _spellTrigger s
isLegal (MonsterStats m) = scale m <= 10 && (m ^. monsterSpells & all ((<= 15) . scale))

isLegalDeck :: [CardStats] -> Bool
isLegalDeck cs =
  length cs >= 40
    && length cs <= 60
    && all isLegal cs
    && sum (map (max 0 . scale) cs) <= 200

--------------------------------------------------------------------------------

-- Existential Quantification
data Ex (c :: Type -> Constraint) where
  Ex :: (c a) => a -> Ex c

type Scale = Int

class HasScale a where
  scale :: a -> Scale

data Trigger = OnPlay | OnDiscard | OnDraw | OnTap | OnDefeat | Infinity deriving (Eq)

instance Show Trigger where
  show OnPlay = "When played"
  show OnDiscard = "When discarded"
  show OnDraw = "When drawn"
  show OnTap = "Tap this card"
  show OnDefeat = "When defeated"
  show Infinity = "On your turn"

isMonsterOnly :: Trigger -> Bool
isMonsterOnly OnTap = True
isMonsterOnly OnDefeat = True
isMonsterOnly Infinity = True
isMonsterOnly _ = False

class (HasScale a, Ord a, Typeable a, Show a) => Requirement a where
  testRequirement :: a -> GameOpWithCardContext Bool
  monsterOnlyRequirement :: a -> Bool
  monsterOnlyRequirement = const False

instance Eq (Ex Requirement) where
  (==) (Ex a) (Ex b) = case cast b of
    Just b' -> a == b'
    Nothing -> False

-- Symmetric => Not an Ordering Relation
instance Ord (Ex Requirement) where
  (<=) (Ex a) (Ex b) = case cast b of
    Just b' -> a <= b'
    Nothing -> False

-- Sugar to for creating requirement lists
(~>) :: (c a, Ord (Ex c)) => OS.OSet (Ex c) -> a -> OS.OSet (Ex c)
(~>) a b = (OS.|>) a (Ex b)

-- This is supposed to be the start of the list
reqs :: (c a) => a -> OS.OSet (Ex c)
reqs = OS.singleton . Ex

-- reqs a ~> b ~> c ~> ...

instance Requirement (Ex Requirement) where
  testRequirement (Ex r) = testRequirement r
  monsterOnlyRequirement (Ex r) = monsterOnlyRequirement r

instance HasScale (Ex Requirement) where
  scale (Ex r) = scale r

instance Show (Ex Requirement) where
  show (Ex r) = show r

class (HasScale a, Show a) => Effect a where
  performEffect :: a -> GameOpWithCardContext ()
  monsterOnlyEffect :: a -> Bool
  monsterOnlyEffect = const False

instance Effect (Ex Effect) where
  performEffect (Ex e) = performEffect e
  monsterOnlyEffect (Ex e) = monsterOnlyEffect e

instance Show (Ex Effect) where
  show (Ex e) = show e

instance HasScale (Ex Effect) where
  scale (Ex e) = scale e

type Effects = [Ex Effect]

type Conditions = OS.OSet (Ex Requirement)

data Spell = Spell
  { _spellName :: String,
    _spellTrigger :: Trigger,
    _castingConditions :: Conditions,
    _effects :: [Ex Effect]
  }

-- Due to a dependency loop at the core of the problem, macros cannot be used to
-- create lenses.

type SpellLens a = Lens' Spell a

spellName :: SpellLens String
spellName = lens _spellName $ \s x -> s {_spellName = x}

spellTrigger :: SpellLens Trigger
spellTrigger = lens _spellTrigger $ \s x -> s {_spellTrigger = x}

castingConditions :: SpellLens Conditions
castingConditions = lens _castingConditions $ \s x -> s {_castingConditions = x}

effects :: SpellLens Effects
effects = lens _effects $ \s x -> s {_effects = x}

data Monster = Monster
  { _monsterName :: String,
    _monsterSpells :: [Spell],
    _summoningConditions :: OS.OSet (Ex Requirement),
    _combatPower :: Natural,
    _isTapped :: Bool
  }

type MonsterLens a = Lens' Monster a

monsterName :: MonsterLens String
monsterName = lens _monsterName $ \m x -> m {_monsterName = x}

monsterSpells :: MonsterLens [Spell]
monsterSpells = lens _monsterSpells $ \m x -> m {_monsterSpells = x}

summoningConditions :: MonsterLens Conditions
summoningConditions = lens _summoningConditions $ \m x -> m {_summoningConditions = x}

combatPower :: MonsterLens Natural
combatPower = lens _combatPower $ \m x -> m {_combatPower = x}

isTapped :: MonsterLens Bool
isTapped = lens _isTapped $ \m x -> m {_isTapped = x}

data CardStats = SpellStats Spell | MonsterStats Monster

spellStats :: Prism' CardStats Spell
spellStats = prism SpellStats $ \case
  SpellStats s -> Right s
  m -> Left m

monsterStats :: Prism' CardStats Monster
monsterStats = prism MonsterStats $ \case
  MonsterStats m -> Right m
  s -> Left s

instance HasScale CardStats where
  scale (SpellStats s) = scale s
  scale (MonsterStats m) = scale m

data Card = Card
  { _cardID :: Natural,
    _cardFamilies :: OS.OSet String,
    _cardStats :: CardStats
  }

type CardLens a = Lens' Card a

cardID :: CardLens Natural
cardID = lens _cardID $ \c x -> c {_cardID = x}

cardFamilies :: CardLens (OS.OSet String)
cardFamilies = lens _cardFamilies $ \c x -> c {_cardFamilies = x}

cardStats :: CardLens CardStats
cardStats = lens _cardStats $ \c x -> c {_cardStats = x}

cardElim :: (Spell -> a) -> (Monster -> a) -> Card -> a
cardElim fs fm c = case _cardStats c of
  SpellStats s -> fs s
  MonsterStats m -> fm m

cardElim' :: (Spell -> GameOpWithCardContext a) -> (Monster -> GameOpWithCardContext a) -> Card -> GameOperation a
cardElim' fs fm c = cardElim (flip runReaderT c . fs) (flip runReaderT c . fm) c

cardName :: Card -> String
cardName = cardElim _spellName _monsterName

isMonster :: Card -> Bool
isMonster = isJust . preview (cardStats % monsterStats)

instance HasScale Card where
  scale c = scale $ _cardStats c

data Player = Player1 | Player2 deriving (Eq, Show)

data PlayerState = PlayerState
  { _hand :: [Card],
    _deck :: [Card],
    -- Only monsters. cardID etc must be retained though
    _field :: [Card],
    _graveyard :: [Card]
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
