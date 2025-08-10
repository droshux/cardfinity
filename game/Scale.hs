module Scale where
import Types (Card, Trigger (..), Spell (..), Monster (..), cardStats, CardStats(..))
import Control.Monad.Except (ExceptT, runExceptT, throwError)
import Control.Monad.Reader (Reader, runReader, asks)
import Data.Functor ((<&>))
import Control.Monad (unless)
import System.Exit (exitFailure)
import Data.Foldable (toList)
import Data.List (find)
import Optics.Operators ((^.))
import Atoms (Condition, Effect)

data LegalityContext = LegalityContext
  { deckContext :: [Card],
    inMonster :: Bool,
    ignoreSTNotFound :: Bool
  }

data LegalityIssue
  = ScaleTooHigh Int Int String
  | MOonSpellable (Maybe Effect, Maybe Condition) String
  | SearchTypeNotFound String -- Text of the searchtype
  | TooManyCards Int
  | TooFewCards Int
  | DeckScaleTooHigh Int

type LegalityCheck = ExceptT LegalityIssue (Reader LegalityContext)

type Scale = LegalityCheck Int

class HasScale a where
  scale :: a -> Scale

runScale :: (HasScale a) => [Card] -> a -> Either LegalityIssue Int
runScale dck x = runReader (runExceptT $ scale x) $ LegalityContext {deckContext = dck, inMonster = False, ignoreSTNotFound = False}

sumScale :: (HasScale a, Traversable t) => t a -> Scale
sumScale = (<&> sum) . mapM scale

punishment :: Int
punishment = 5

sumWithPunishment :: (HasScale a, Traversable t) => Int -> t a -> Scale
sumWithPunishment mul xs = do
  total <- sumScale xs
  let count = max 0 (length xs - 1)
  return $ total + mul * count * punishment

deckLegal :: LegalityCheck ()
deckLegal = do
  -- Decks must have 40-60 cards
  len <- asks $ length . deckContext
  unless (len >= 40) $ throwError $ TooFewCards len
  unless (len <= 60) $ throwError $ TooManyCards len

  -- Decks must have a total scale of 20 or less.
  total <- asks deckContext >>= sumScale
  unless (total <= 200) $ throwError $ DeckScaleTooHigh total


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

instance HasScale Trigger where
  scale Infinity = return 50
  scale OnPlay = return 0
  scale OnAttach = return 0
  scale _ = return 5

instance HasScale Card where
  scale c = scale $ c ^. cardStats 

instance HasScale CardStats where
  scale (SpellStats s) = scale s
  scale (MonsterStats m) = scale m

instance HasScale Spell where
  scale (Spell n t r e) = do
    -- Some requirements and effects only make sense for monsters,
    -- these cannot be used on spells that can be cast from the hand.
    spellable <- asks ((&& not (isMonsterOnly t)) . not . inMonster)
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

