module Scale where

import Atoms
import Control.Monad (unless, when)
import Control.Monad.Except (ExceptT, runExceptT, throwError)
import Control.Monad.Reader (MonadReader (local), Reader, asks, runReader)
import Data.Foldable (toList)
import Data.Functor ((<&>))
import Data.List (find)
import Data.List.NonEmpty qualified as NonE
import GHC.Natural (Natural)
import GameUtils (toPredicate)
import Optics.Operators ((^.))
import System.Exit (exitFailure)
import Types
import Utils (natToInt)

punishment :: Int
punishment = 5

instance HasScale Condition where
  scale (Destroy d f) = do
    let multiplier = (if isField f then 15 else 10) + (if d == Banish then 2 else 0)
    let n = natToInt (getCount f)
    st <- scale (getSearchType f)
    return $ -(n * multiplier + st)
  scale DiscardSelf = return $ -4
  scale (TakeDamage n False) = let i = natToInt n in return $ -(i * coreFn i)
  scale (TakeDamage n True) = let i = natToInt n in return $ -(i * (coreFn i + 2))
  scale (HealOpponent n) = scale (Heal n) <&> (\x -> -x)
  scale (Pop n) = return $ -(2 * natToInt n)
  scale (YouMay cond) = scale cond <&> (+ 2)
  scale (Choose cs) = mapM scale (NonE.toList cs) <&> maximum

instance HasScale Effect where
  scale (DestroyEnemy d f) = return $ natToInt $ destroyEnemyScale d f
  scale DiscardEnemy = scale DiscardSelf <&> (3 -)
  scale (DealDamage n isTrue) = let mult = if isTrue then 7 else 5 in return $ mult * natToInt n
  scale (Heal n) = return $ 7 * natToInt n
  scale DECKOUT = return $ -punishment
  scale (Draw n) = return $ natToInt n * 10
  scale (Peek n) = return $ 2 ^ n
  scale (Scry n) = scale (Peek n)
  scale (Optional e) = scale e <&> max (-punishment)
  scale (ChooseEffect es) = mapM scale (NonE.toList es) <&> (+ length es) . maximum
  scale (Attack piercing) = return $ if piercing then 20 else 10
  scale (Play t) = case t of
    ForSpell -> return $ -3
    o -> scale o
  scale (Search (SearchFor ForSpell)) = return 10
  scale (Search (SearchFor _)) = return 15
  scale (Search (DrillFor t)) = do
    notFound <- asks ((==) 0 . countMatches t . deckContext)
    when notFound $ throwError $ SearchTypeNotFound (show t)
    return 0
  scale (Attach t) = scale t <&> (+ 5)
  scale (Buff by forItself) = return $ max (-punishment) $ if forItself then 2 * fromIntegral by else 3 * fromIntegral by
  scale (AsEffect cond) = scale cond

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

    -- Double punishment for additional monster spells
    spells <- local (\c -> c {inMonster = True}) (sumWithPunishment 2 ss)
    let power = fromIntegral p * length (show p) -- Multiply by number of digits
    let tap = if t && anyTap ss then -5 else 0 -- Enters the field tapped
    let total = requirements + spells + power + tap

    -- Monsters must have scale of 10 or less
    unless (total <= 10) $ throwError $ ScaleTooHigh 10 total n
    return total
    where
      anyTap = any ((OnTap ==) . (^. spellTrigger))

instance HasScale Trigger where
  scale Infinity = return 20
  scale Counter = return 20
  scale OnPlay = return 0
  scale OnAttach = return 0
  scale _ = return 5

destroyEnemyScale :: DestroyType -> FindCards -> Natural
destroyEnemyScale Discard (FindCardsHand n _) = 10 * n + 2
destroyEnemyScale Banish (FindCardsHand n _) = 12 * n + 2
destroyEnemyScale Discard (FindCardsField n _) = n * 15 + 2
destroyEnemyScale Banish (FindCardsField n _) = n * 17 + 2

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

instance HasScale Card where
  scale c = scale $ c ^. cardStats

instance HasScale CardStats where
  scale (SpellStats s) = scale s
  scale (MonsterStats m) = scale m

-- Decreases from 5->1 as input doubles eg: f 1 = 5, f 2 = 4, f 4 = 3, f 8 = 2,
-- f 16 = 1,... (minimum 1)
coreFn :: Int -> Int
coreFn x
  | x == 1 = 5
  | x < 4 = 4
  | x < 8 = 3
  | x < 16 = 2
  | otherwise = 1

countMatches :: SearchType -> [Card] -> Int
countMatches st = length . filter (toPredicate st)

instance HasScale SearchType where
  scale t = asks deckContext >>= calcRarity . length . filter (toPredicate t)
    where
      calcRarity :: Int -> Scale
      calcRarity x
        | x == 0 = do
            ignore <- asks ignoreSTNotFound
            unless ignore $ throwError $ SearchTypeNotFound $ show t
            return 0
        | otherwise = return $ 2 ^ (coreFn x - 1)

rarity :: [Card] -> SearchType -> String
rarity dck st = discLog2 $ countMatches st dck
  where
    discLog2 x
      | x == 1 = "Legendary"
      | x == 2 = "Very Rare"
      | x <= 4 = "Rare"
      | x <= 8 = "Uncommon"
      | x <= 16 = "Common"
      | otherwise = "Very Common"

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
