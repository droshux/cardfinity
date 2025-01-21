module Types where

import Control.Monad.Except
import Control.Monad.Reader (ReaderT (runReaderT))
import Control.Monad.State
import Data.Data (Typeable, cast)
import Data.Foldable (Foldable (toList))
import Data.Kind (Constraint, Type)
import Data.Set.Ordered qualified as OS (OSet, singleton, (|>))
import GHC.Natural (Natural)

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
      + if ut && t then -5 else 0 -- Enters the field tapped
    where
      ut = any ((==) OnTap . spellTrigger) ss
      sp v =
        let f :: Float = fromIntegral v
            sc :: Scale = fromIntegral v
         in sc * ceiling (logBase 10.0 f)

isLegal :: CardStats -> Bool
isLegal (SpellStats s) =
  -- MonsterOnly effects cannot appear on spells that could be played as cards
  scale s <= 10 && (mot || (not moe && not mor))
  where
    moe = any monsterOnlyEffect $ effects s
    mor = any monsterOnlyRequirement $ castingConditions s
    mot = isMonsterOnly $ spellTrigger s
isLegal (MonsterStats m) = scale m <= 10 && all ((<= 15) . scale) (monsterSpells m)

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

data Spell = Spell
  { spellName :: String,
    spellTrigger :: Trigger,
    castingConditions :: OS.OSet (Ex Requirement),
    effects :: [Ex Effect]
  }

data Monster = Monster
  { monsterName :: String,
    monsterSpells :: [Spell],
    summoningConditions :: OS.OSet (Ex Requirement),
    combatPower :: Natural,
    isTapped :: Bool
  }

data CardStats = SpellStats Spell | MonsterStats Monster

instance HasScale CardStats where
  scale (SpellStats s) = scale s
  scale (MonsterStats m) = scale m

data Card = Card
  { cardID :: Natural,
    cardFamilies :: OS.OSet String,
    cardStats :: CardStats
  }

cardElim :: (Spell -> a) -> (Monster -> a) -> Card -> a
cardElim fs fm c = case cardStats c of
  SpellStats s -> fs s
  MonsterStats m -> fm m

cardElim' :: (Spell -> GameOpWithCardContext a) -> (Monster -> GameOpWithCardContext a) -> Card -> GameOperation a
cardElim' fs fm c = cardElim (flip runReaderT c . fs) (flip runReaderT c . fm) c

cardName :: Card -> String
cardName = cardElim spellName monsterName

isMonster :: Card -> Bool
isMonster = cardElim (const False) (const True)

instance HasScale Card where
  scale c = scale $ cardStats c

data Player = Player1 | Player2 deriving (Eq, Show)

data PlayerState = PlayerState
  { hand :: [Card],
    deck :: [Card],
    -- Only monsters. cardID etc must be retained though
    field :: [Card],
    graveyard :: [Card]
  }

data CardLocation = Hand | Deck | Field | Graveyard deriving (Eq, Show, Ord, Enum)

allCardLocations :: [CardLocation]
allCardLocations = enumFrom $ toEnum 0

toLens :: CardLocation -> PlayerState -> [Card]
toLens Hand = hand
toLens Deck = deck
toLens Field = field
toLens Graveyard = graveyard

data GameState = GameState
  { player1State :: PlayerState,
    player2State :: PlayerState,
    isFirstTurn :: Bool
  }

getPlayerState :: Player -> GameState -> PlayerState
getPlayerState Player1 = player1State
getPlayerState Player2 = player2State

{- Perform an operation for a given player, returning a player early if they
deckout, keeping track of the game and taking user IO -}
type GameOperation = ReaderT Player (ExceptT Player (StateT GameState IO))

type GameOpWithCardContext = ReaderT Card GameOperation
