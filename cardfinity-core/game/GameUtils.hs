{-# LANGUAGE LambdaCase #-}
{-# HLINT ignore "Redundant <&>" #-}
{-# OPTIONS_GHC -Wno-missing-signatures #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module GameUtils
  ( asOpponent,
    asOpponent',
    selectFromList,
    selectFromList',
    selectFromListCancelable,
    selectFromListCancelable',
    selectFromListNoPlayer,
    selectFromListNoPlayer',
    ifNotCancelled,
    cancelFallback,
    Cancelable (..),
    deckout,
    shuffleDeck,
    tapThisCard,
    findThisCard,
    lensThisCard,
    printCardsIn,
    (.=),
    (=:),
    (%=),
    (-=),
    player's,
    opponent's,
    player's',
    toPredicate,
    getLocation,
    sandbox,
  )
where

import Atoms (FindCards (..), SearchType (..))
import Control.Monad (mfilter, (<=<))
import Control.Monad.Except (runExceptT, throwError)
import Control.Monad.Reader (MonadIO (liftIO), MonadReader (ask, local), MonadTrans (lift), ReaderT (runReaderT), asks)
import Control.Monad.State (MonadState (get), StateT (runStateT), gets, modify)
import Data.Bifunctor (second)
import Data.Foldable (Foldable (toList))
import Data.Functor ((<&>))
import Data.List (findIndex)
import Data.List.NonEmpty (NonEmpty ((:|)))
import Data.Maybe (mapMaybe)
import Optics (AffineTraversal', Ixed (ix), Lens', atraversal, over, preview, set, view, (%), (^.))
import System.Random.Shuffle (shuffleM)
import Text.Read (readMaybe)
import Types
import Utils (collapse, showFold, without)

toPredicate :: SearchType -> Card -> Bool
toPredicate (ForName n) = (n ==) . cardName
toPredicate (ForFamily f) = elem f . (^. cardFamilies)
toPredicate ForCard = const True
toPredicate t = cardElim (const $ t == ForSpell) (const $ t == ForMonster)

getLocation (FindCardsField _ _) = field
getLocation (FindCardsHand _ _) = hand

sandbox :: GameOpWithCardContext a -> GameOpWithCardContext (Either Player a, GameState)
sandbox op = do
  context <- ask
  currentPlayer <- lift ask
  initialState <- get
  let runReaders = flip runReaderT currentPlayer . flip runReaderT context
  let runMisc = flip runStateT initialState . runExceptT
  liftIO $ runMisc $ runReaders op

asOpponent :: (MonadReader Player m) => m a -> m a
asOpponent op = do
  p <- ask
  let p' = case p of Player1 -> Player2; Player2 -> Player1
  local (const p') op

asOpponent' :: GameOpWithCardContext a -> GameOpWithCardContext a
asOpponent' op = ask >>= lift . asOpponent . runReaderT op

selectFromListNoPlayer :: (Show a) => String -> NonEmpty a -> GameOperation (Int, a)
selectFromListNoPlayer _ (option :| []) = return (0, option)
selectFromListNoPlayer prompt options = do
  liftIO $ putStrLn prompt
  helper $ toList options
  where
    printOptions = mapM_ printOption . zip [0 ..]
    helper xs = do
      liftIO $ printOptions xs
      liftIO getLine <&> mfilter (inRange xs) . readMaybe >>= \case
        Just i -> return (i, xs !! i)
        Nothing -> do
          liftIO $ putStrLn "Invalid input, please try again..."
          helper xs
    printOption (i :: Int, s) = putStrLn (show i ++ ": " ++ show s)
    inRange xs i = 0 <= i && i < length xs

selectFromList :: (Show a) => String -> NonEmpty a -> GameOperation (Int, a)
selectFromList prompt options = do
  ask >>= liftIO . putStr . (++ ", ") . show
  selectFromListNoPlayer prompt options

selectFromListNoPlayer' :: (Show a) => String -> NonEmpty a -> GameOpWithCardContext (Int, a)
selectFromListNoPlayer' prompt = lift . selectFromListNoPlayer prompt

selectFromList' :: (Show a) => String -> NonEmpty a -> GameOpWithCardContext (Int, a)
selectFromList' prompt = lift . selectFromList prompt

data Cancelable a where
  Cancel :: Cancelable a
  Option :: (Show a) => a -> Cancelable a

instance Show (Cancelable a) where
  show (Option x) = show x
  show Cancel = "Cancel"

selectFromListCancelable :: (Show a) => String -> [a] -> GameOperation (Cancelable (Int, a))
selectFromListCancelable prompt = return . h <=< selectFromList prompt . (Cancel :|) . map Option
  where
    h (_, Cancel) = Cancel
    -- Cancel is always 0, users choice is always 1 more than the index.
    h (i, Option x) = Option (i - 1, x)

selectFromListCancelable' :: (Show a) => String -> [a] -> GameOpWithCardContext (Cancelable (Int, a))
selectFromListCancelable' prompt = lift . selectFromListCancelable prompt

ifNotCancelled :: (MonadIO m) => Cancelable a -> (a -> m ()) -> m ()
ifNotCancelled c f = cancelFallback c (liftIO $ putStrLn "Cancelled") f

cancelFallback :: (MonadIO m) => Cancelable a -> m b -> (a -> m b) -> m b
cancelFallback c fb f = case c of
  Cancel -> fb
  Option p -> f p

-- Set a lens on the current player
(.=) optic x = asks playerLens >>= modify . flip set x . (% optic)

-- Update a lens on the current plater
(%=) optic f = asks playerLens >>= modify . flip over f . (% optic)

-- Cons a lens
(=:) optic v = (%=) optic (v :)

-- Without a lens
(-=) optic = (%=) optic . flip without

-- player's :: Lens' PlayerState a -> GameOperation a
player's lens = asks playerLens >>= gets . view . (% lens)

-- player's' :: Lens' PlayerState a -> GameOpWithCardContext a
player's' = lift . player's

opponent's :: Lens' PlayerState a -> GameOperation a
opponent's = asOpponent . player's

deckout :: GameOperation a
deckout = ask >>= throwError

shuffleDeck :: GameOperation ()
shuffleDeck = player's deck >>= shuffleM >>= (deck .=)

tapThisCard :: GameOpWithCardContext ()
tapThisCard = do
  this <- lensThisCard
  lift $ this % monsterStats % isTapped .= True

findThisCard :: GameOpWithCardContext (Maybe (Int, CardLocation))
findThisCard = mapM findThisIn allCardLocations <&> fmap (second toEnum) . firstIndex 0
  where
    firstIndex _ [] = Nothing
    firstIndex i (Nothing : xs) = firstIndex (i + 1) xs
    firstIndex i ((Just x) : _) = Just (x, i)
    findThisIn loc = do
      cid <- asks (^. cardID)
      player's' (toLens loc) <&> findIndex (\c -> c ^. cardID == cid)

lensThisCard :: GameOpWithCardContext (AffineTraversal' PlayerState Card)
lensThisCard =
  findThisCard <&> \case
    Nothing -> atraversal Left const
    Just (i, loc) -> toLens loc % ix i

printCardsIn :: String -> CardLocation -> GameOperation ()
printCardsIn delim Graveyard = do
  cards <- player's graveyard
  liftIO $ putStrLn ("(" ++ show (length cards) ++ " cards)")
  liftIO $ putStrLn $ showFold delim $ map cardName cards
printCardsIn delim Field = player's field >>= liftIO . putStrLn . delimFold . collapse . mapMaybe (preview monsterStats)
  where
    fieldPrint (m, i) =
      let b = if m ^. isTapped then "'" else "\""
          p = if m ^. combatPower > 0 then " (" ++ show (m ^. combatPower) ++ ")" else ""
          x = if i > 1 then show i ++ "x " else ""
       in x ++ b ++ m ^. monsterName ++ p ++ b
    delimFold [] = ""
    delimFold [c] = fieldPrint c
    delimFold (c : cs) = fieldPrint c ++ delim ++ delimFold cs
printCardsIn delim l = player's (toLens l) >>= liftIO . putStrLn . showFold delim . map cardName

instance Show Spell where
  show (Spell n t cs es) = concat [show n, " ", show t, if null cs then ": " else scs, ses]
    where
      scs = " " ++ showFold ", " (toList cs) ++ ": "
      ses = showFold ", " es

instance Show Monster where
  show (Monster n ss rs p t) =
    concat $
      show n
        : ( if null rs
              then []
              else
                [ "\n",
                  showFold ", " $ toList rs,
                  ":"
                ]
          )
        ++ map (\(s, c) -> "\n\t" ++ (if c > 1 then show c ++ "x " else "") ++ show s) (collapse ss)
        ++ [ "\n\tPower ",
             show p,
             if t then "\t[Tapped]" else ""
           ]

instance Show CardStats where
  show (SpellStats s) = show s
  show (MonsterStats s) = show s

instance Show Card where
  show (Card _ fs cs _) =
    concat $
      show cs
        : if null fs
          then []
          else
            [ "\n\t(",
              showFold ", " $ toList fs,
              ")"
            ]
