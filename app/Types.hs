module Types where

-- For typeclass objects.
import Control.Monad.Except
import Control.Monad.Reader (ReaderT)
import Control.Monad.State
import Data.Kind (Constraint, Type)
import GHC.Natural (Natural)

-- Existential Quantification
data Ex (c :: Type -> Constraint) where
  Ex :: (c a) => a -> Ex c

type Scale = Int

class HasScale a where
  scale :: a -> Scale

data Trigger = OnPlay | OnDiscard | OnDraw | OnTap | OnDefeat | Infinity deriving (Eq, Show)

isMonsterOnly :: Trigger -> Bool
isMonsterOnly OnTap = True
isMonsterOnly OnDefeat = True
isMonsterOnly Infinity = True
isMonsterOnly _ = False

instance HasScale Trigger where
  scale Infinity = 50
  scale OnPlay = 0
  scale _ = 5

class (HasScale a) => Requirement a where
  testRequirement :: Card -> a -> GameOperation Bool

instance Requirement (Ex Requirement) where
  testRequirement c (Ex r) = testRequirement c r

instance HasScale (Ex Requirement) where
  scale (Ex r) = scale r

class (HasScale a) => Effect a where
  performEffect :: Card -> a -> GameOperation ()

instance Effect (Ex Effect) where
  performEffect c (Ex e) = performEffect c e

instance HasScale (Ex Effect) where
  scale (Ex e) = scale e

data Spell = Spell
  { spellName :: String,
    spellTrigger :: Trigger,
    castingConditions :: [Ex Requirement],
    effects :: [Ex Effect]
  }

punishment :: Scale
punishment = 10

instance HasScale Spell where
  scale (Spell _ t c e) =
    scale t
      + sum (map scale c)
      + sum (map scale e)
      + punishment * max 0 (length e - 1)

data Monster = Monster
  { monsterName :: String,
    monsterSpells :: [Spell],
    summoningConditions :: [Ex Requirement],
    combatPower :: Natural,
    isTapped :: Bool
  }

instance HasScale Monster where
  scale (Monster _ s c p t) = _

data CardStats = SpellStats Spell | MonsterStats Monster

instance HasScale CardStats where
  scale (SpellStats s) = scale s
  scale (MonsterStats m) = scale m

data Card = Card
  { cardID :: Int,
    cardFamilies :: [String],
    cardStats :: CardStats
  }

cardElim :: (Spell -> a) -> (Monster -> a) -> Card -> a
cardElim fs fm c = case cardStats c of
  SpellStats s -> fs s
  MonsterStats m -> fm m

instance HasScale Card where
  scale c = scale $ cardStats c

data Player = Player1 | Player2 deriving (Eq, Show)

data PlayerState = PlayerState
  { hand :: [Card],
    deck :: [Card],
    field :: [Monster],
    graveyard :: [Card]
  }

data CardLocation = Hand | Deck | Field | Graveyard deriving (Eq, Show)

toLens :: CardLocation -> PlayerState -> [Ex HasScale]
toLens Hand = map Ex . hand
toLens Deck = map Ex . deck
toLens Field = map Ex . field
toLens Graveyard = map Ex . graveyard

data GameState = GameState
  { player1State :: PlayerState,
    player2State :: PlayerState,
    isFirstTurn :: Bool
  }

{- Perform an operation for a given player, returning a player early if they
deckout, keeping track of the game and taking user IO -}
type GameOperation = ReaderT Player (ExceptT Player (StateT GameState IO))
