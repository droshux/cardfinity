{-# LANGUAGE LambdaCase #-}

module Types
  ( punishment,
    isLegal,
    Scale,
    runScale,
    HasScale (..),
    LegalityContext (..),
    LegalityIssue (..),
    Trigger (..),
    Requirement (..),
    Conditions,
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
    isFirstTurn,
    player1State,
    player2State,
    playerLens,
  )
where

import Control.Monad.Except
import Control.Monad.RWS (MonadReader (local), asks)
import Control.Monad.Reader (Reader, ReaderT (runReaderT), runReader)
import Control.Monad.State
import Data.Default (Default (def))
import Data.Foldable (Foldable (toList), find)
import Data.Maybe (isJust)
import Data.Set.Ordered qualified as OS (OSet)
import GHC.Natural (Natural)
import Optics
import System.Exit (exitFailure)
import Control.Monad (unless, when)

--------------------------------------------------------------------------------
-- CARD BALANCING AREA:
punishment :: Int
punishment = 5

instance HasScale Trigger where
  scale Infinity = return 50
  scale OnPlay = return 0
  scale OnAttach = return 0
  scale _ = return 5

instance HasScale Spell where
  scale (Spell n t r e) = do
    -- Some requirements and effects only make sense for monsters,
    -- these cannot be used on spells that can be cast from the hand.
    spellable <- asks (not . inMonster) <&> (&& not (isMonsterOnly t))
    case (find monsterOnlyEffect e, find monsterOnlyRequirement r) of
      (Nothing, Nothing) -> return ()
      bad -> when spellable $ throwError $ MOonSpellable bad n

    ts <- scale t
    rs <- sumScale $ toList r
    es <- sumWithPunishment 1 e
    let total = ts + rs + es

    -- Monster spells must be 15 scale or less but spell cards
    -- must be 10 scale or less.
    limit <- asks inMonster >>= \m -> return $ if m then 15 else 10
    unless (total <= limit) $ throwError $ ScaleTooHigh limit total n

    return total

instance HasScale Monster where
  scale (Monster n ss r p t) = do
    requirements <- sumScale $ toList r

    -- Double punishment for monster spells
    spells <- local (\c -> c {inMonster = True}) (sumWithPunishment 2 ss)
    let power = fromIntegral p * length (show p) -- Multiply by number of digits
    let tap = if t && anyTap ss then -5 else 0 -- Enters the field tapped
    let total = requirements + spells + power + tap

    -- Monsters must have scale of 10 or less
    unless (total <= 10) $ throwError $ ScaleTooHigh 10 total n
    return total
    where
      anyTap = any ((OnTap ==) . (^. spellTrigger))

deckLegal :: LegalityCheck ()
deckLegal = do
  -- Decks must have 40-60 cards
  len <- asks $ length . deckContext
  unless (len >= 40) $ throwError $ TooFewCards len
  unless (len <= 60) $ throwError $ TooManyCards len

  -- Decks must have a total scale of 20 or less.
  total <- asks deckContext >>= sumScale
  unless (total <= 200) $ throwError $ DeckScaleTooHigh total

--------------------------------------------------------------------------------

data LegalityIssue
  = ScaleTooHigh Int Int String
  | MOonSpellable (Maybe Effect, Maybe Requirement) String
  | SearchTypeNotFound String -- Text of the searchtype
  | TooManyCards Int
  | TooFewCards Int
  | DeckScaleTooHigh Int

instance Show LegalityIssue where
  show (ScaleTooHigh limit s name) =
    concat
      [ "Scale of ",
        show name,
        " must be ",
        show limit,
        " or less! (Currently: ",
        show s,
        ")"
      ]
  show (DeckScaleTooHigh s) =
    concat
      [ "The total scale of a deck must be 200 or less! (Currently: ",
        show s,
        ")"
      ]
  show (TooManyCards n) =
    concat
      [ "A deck must have 60 cards or fewer! (Currently: ",
        show n,
        ")"
      ]
  show (TooFewCards n) =
    concat
      [ "A deck must have at least 40 cards! (Currently: ",
        show n,
        ")"
      ]
  show (SearchTypeNotFound s) = "No cards matching " ++ s ++ " found!"
  show (MOonSpellable (Just e, _) n) =
    concat
      [ show n,
        " can be cast from the hand but effect \"",
        show e,
        "\" can only be part of a monster."
      ]
  show (MOonSpellable (_, Just r) n) =
    concat
      [ show n,
        " can be cast from the hand but requirement \"",
        show r,
        "\" can only be part of a monster."
      ]
  show (MOonSpellable (Nothing, Nothing) n) = "Woops! Error thrown even though " ++ show n ++ " is MOE legal!"

data LegalityContext = LegalityContext
  { deckContext :: [Card],
    inMonster :: Bool,
    ignoreSTNotFound :: Bool
  }

type LegalityCheck = ExceptT LegalityIssue (Reader LegalityContext)

type Scale = LegalityCheck Int

runScale :: (HasScale a) => [Card] -> a -> Either LegalityIssue Int
runScale dck x = runReader (runExceptT $ scale x) $ LegalityContext {deckContext = dck, inMonster = False, ignoreSTNotFound = False}

sumScale :: (HasScale a, Traversable t) => t a -> Scale
sumScale = (<&> sum) . mapM scale

sumWithPunishment :: (HasScale a, Traversable t) => Int -> t a -> Scale
sumWithPunishment mul xs = do
  total <- sumScale xs
  let count = max 0 (length xs - 1)
  return $ total + mul * count * punishment

-- Throws with nice error message if the input is an illegal deck.
isLegal :: [Card] -> IO [Card]
isLegal dck = do
  let ctex = LegalityContext {deckContext = dck, inMonster = False, ignoreSTNotFound = False}
  case runReader (runExceptT deckLegal) ctex of
    Left err -> do
      putStrLn "Illegal deck:"
      print err
      exitFailure
    Right () -> return dck

class HasScale a where
  scale :: a -> Scale

data Trigger = OnPlay | OnDiscard | OnDraw | OnTap | OnVictory | OnDefeat | OnAttach | Infinity deriving (Eq)

instance Show Trigger where
  show OnPlay = "When played"
  show OnDiscard = "When discarded"
  show OnDraw = "When drawn"
  show OnTap = "Tap this card"
  show OnDefeat = "When defeated"
  show OnVictory = "When defeating a monster"
  show OnAttach = "When this spell is attached to a monster"
  show Infinity = "On your turn"

isMonsterOnly :: Trigger -> Bool
isMonsterOnly OnTap = True
isMonsterOnly OnDefeat = True
isMonsterOnly OnVictory = True
isMonsterOnly OnAttach = True
isMonsterOnly Infinity = True
isMonsterOnly _ = False

data Requirement = Requirement
  { testRequirement :: GameOpWithCardContext Bool,
    monsterOnlyRequirement :: Bool,
    requirementScale :: Scale,
    displayRequirement :: String
  }

instance HasScale Requirement where
  scale = requirementScale

instance Show Requirement where
  show = displayRequirement

instance Eq Requirement where
  (==) r1 r2 = displayRequirement r1 == displayRequirement r2

instance Ord Requirement where
  (<=) r1 r2 = displayRequirement r1 <= displayRequirement r2

instance Default Requirement where
  def =
    Requirement
      { testRequirement = return False,
        monsterOnlyRequirement = False,
        requirementScale = return 0,
        displayRequirement = "Always"
      }

data Effect = Effect
  { performEffect :: GameOpWithCardContext (),
    monsterOnlyEffect :: Bool,
    effectScale :: Scale,
    displayEffect :: String
  }

instance HasScale Effect where
  scale = effectScale

instance Show Effect where
  show = displayEffect

instance Default Effect where
  def =
    Effect
      { performEffect = return (),
        monsterOnlyEffect = False,
        effectScale = return 0,
        displayEffect = "Do nothing"
      }

type Effects = [Effect]

type Conditions = OS.OSet Requirement

data Spell = Spell
  { _spellName :: String,
    _spellTrigger :: Trigger,
    _castingConditions :: Conditions,
    _effects :: Effects
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
    _summoningConditions :: Conditions,
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

prismSpellStats :: Prism' CardStats Spell
prismSpellStats = prism SpellStats $ \case
  SpellStats s -> Right s
  m -> Left m

prismMonsterStats :: Prism' CardStats Monster
prismMonsterStats = prism MonsterStats $ \case
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

instance HasScale Card where
  scale c = scale $ _cardStats c

data Player = Player1 | Player2 deriving (Eq, Show)

otherPlayer :: Player -> Player
otherPlayer Player1 = Player2
otherPlayer Player2 = Player1

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
