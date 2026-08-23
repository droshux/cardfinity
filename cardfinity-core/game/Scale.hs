module Scale (HasScale, isLegal, rarity, runScale,LegalityIssue(..)) where

import AtomDisplay ()
import Atoms
import Control.Monad (unless, when)
import Control.Monad.Except (ExceptT, runExceptT, throwError)
import Control.Monad.Reader (MonadReader (local), Reader, asks, runReader)
import Data.Foldable (toList)
import Data.Functor ((<&>))
import Data.List (find)
import Data.List.NonEmpty qualified as NonE
import GHC.Natural (Natural)
import GHC.Num (integerFromWord, integerLogBase, integerToInt)
import GameUtils (toPredicate)
import Optics.Operators ((^.))
import System.Exit (exitFailure)
import Types
import Utils (natToInt)

punishment :: Int
punishment = 5

-- The inherent value of something being an (untapped) monster
inherent :: Int
inherent = 5

instance HasScale Condition where
  scale (Destroy d f) = do
    let multiplier = fieldMult f + (if d == Banish then 2 else 0)
    let n = natToInt (getCount f)
    -- Half scale from rarity when discarding.
    st <- (`div` (if d == Banish then 1 else 2)) <$> scale (getSearchType f)
    return $ -(n * multiplier + st)
    where
      fieldMult (FindCardsField _ True _) = 8 + inherent
      fieldMult _ = 8
  scale DiscardSelf = (+ 1) <$> scale (TakeDamage 1 False)
  scale (TakeDamage n False) = return $ -natToInt n 
  scale (TakeDamage n True) = do
    true <- scale (TakeDamage n False)
    pop <- scale (Pop n)
    return $ true + pop
  scale (HealOpponent n) = (* (-1)) <$> scale (Heal n)
  scale (Pop n) = return $ -natToInt n
  scale (YouMay cond) = scale cond <&> (+ 2)
  scale (Choose cs) =
    let list = NonE.toList cs
     in mapM scale list <&> (+ length list) . maximum

instance HasScale Effect where
  scale (DestroyEnemy d f) = return $ natToInt $ destroyEnemyScale d f
  scale DiscardEnemy = (* (-1)) <$> scale DiscardSelf
  scale (DealDamage n isTrue) = return $ (if isTrue then 4 else 3) * natToInt n 
  scale (Heal n) = do
    damage <- scale (TakeDamage n False)
    info <- scale (Peek n)
    return $ info - damage
  scale DECKOUT = return 0
  scale (Draw n) = return $ natToInt n * 10
  scale (Peek n) = return $ 2 * natToInt n
  scale (Scry n) = scale (Peek n)
  scale (Optional e) = scale e
  scale (ChooseEffect es) = mapM scale (NonE.toList es) <&> (+ length es) . maximum
  scale (Attack piercing) = return $ if piercing then 15 else 5
  scale (Play t) = case t of
    ForSpell -> return 0
    o -> scale o
  scale (Search (SearchFor t)) = do
    s <- scale t
    return $ 20 - (s `div` 2)
  scale (Search (DrillFor t)) = do
    notFound <- asks ((==) 0 . countMatches t . deckContext)
    when notFound $ throwError $ SearchTypeNotFound (show t)
    return 0
  scale (Attach t) = scale t <&> (+ 5)
  scale (Buff by forItself) = return $ if forItself then 2 * fromIntegral by else 3 * fromIntegral by
  scale (AsEffect cond) = scale cond

class (HasScale a) => Punishable a where
  incursPunishment :: a -> Bool

instance Punishable Effect where
  incursPunishment DECKOUT = False
  incursPunishment (Play ForSpell) = False
  incursPunishment (AsEffect _) = False
  incursPunishment _ = True

instance Punishable Spell where
  incursPunishment _ = True

instance HasScale Spell where
  scale spell = do
    -- Some requirements and effects only make sense for monsters,
    -- these cannot be used on spells that can be cast from the hand.
    spellable <- asks ((&& not (isMonsterOnly (spell ^. spellTrigger))) . not . inMonster)
    case (find monsterOnlyEffect $ spell ^. effects, find monsterOnlyRequirement $ spell ^. castingConditions) of
      (Nothing, Nothing) -> return ()
      bad -> when spellable $ throwError $ MOonSpellable bad $ spell ^. spellName

    ts <- scale $ spell ^. spellTrigger
    rs <- sumScale $ toList $ spell ^. castingConditions
    es <- sumWithPunishment 1 $ spell ^. effects
    let total = ts + rs + es

    -- Monster spells have no scale limit but spell cards
    -- must be 10 scale or less. This only applies to monster spells that start
    -- attached to monsters.
    limit <- asks $ \ctx -> if isMonsterOnly (spell ^. spellTrigger) && inMonster ctx then -1 else 10
    unless (limit < 0 || total <= limit) $ throwError $ ScaleTooHigh limit total $ spell ^. spellName

    return total

instance HasScale Monster where
  scale monster = do
    requirements <- sumScale $ toList $ monster ^. summoningConditions

    -- Double punishment for additional monster spells
    spells <- local (\c -> c {inMonster = True}) (sumWithPunishment 2 $ monster ^. monsterSpells)
    let power = fromIntegral (monster ^. combatPower) * length (show $ monster ^. combatPower) -- Multiply by number of digits
    let tap = if monster ^. entersTapped then -inherent else 0 -- Enters the field tapped
    let total = inherent + requirements + spells + power + tap

    -- Monsters must have scale of 10 or less
    unless (total <= 10) $ throwError $ ScaleTooHigh 10 total $ monster ^. monsterName
    return total

instance HasScale Trigger where
  scale Infinity = return 20
  scale Counter = return 20
  scale OnPlay = return 0
  scale OnAttach = return 0
  scale OnTap = return 10
  scale _ = return 5

destroyEnemyScale :: DestroyType -> FindCards -> Natural
destroyEnemyScale Discard (FindCardsHand n _) = 10 * n + 2
destroyEnemyScale Banish (FindCardsHand n _) = 12 * n + 2
destroyEnemyScale Discard (FindCardsField n False _) = n * 15 + 2
destroyEnemyScale Banish (FindCardsField n False _) = n * 17 + 1
destroyEnemyScale Discard (FindCardsField n True _) = n * 10 + 2
destroyEnemyScale Banish (FindCardsField n True _) = n * 12 + 2

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

sumWithPunishment :: (Punishable a) => Int -> [a] -> Scale
sumWithPunishment _ [] = return 0
sumWithPunishment mul (x : xs) = do
  let punished = length $ filter incursPunishment xs
  total <- sumScale (x : xs)
  return $ total + mul * punished * punishment

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
  scale = cardStatsElim scale scale

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
        | x <= 4 = return ([0, 32, 16, 8, 4] !! x)
        | otherwise = return 0

rarity :: [Card] -> SearchType -> String
rarity dck st = discLog2 $ countMatches st dck
  where
    discLog2 x
      | x == 1 = "Legendary"
      | x == 2 = "Extremely Rare"
      | x == 3 = "Very Rare"
      | x <= 4 = "Rare"
      | otherwise = "Common"

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
