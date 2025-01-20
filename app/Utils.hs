{-# LANGUAGE LambdaCase #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Redundant <&>" #-}

module Utils where

import Control.Monad ((<=<))
import Control.Monad.Except (MonadIO (liftIO), MonadTrans (lift), runExceptT, throwError, void, when)
import Control.Monad.Reader (MonadReader (ask, local), ReaderT (runReaderT), asks)
import Control.Monad.State (MonadState (get, put), StateT (runStateT), gets, modify)
import Data.Foldable (Foldable (toList))
import Data.Functor ((<&>))
import Data.List (findIndex)
import Data.List.NonEmpty (NonEmpty ((:|)))
import Data.Set.Ordered (OSet)
import GHC.Natural (Natural, naturalToInteger)
import GHC.Num (integerToInt)
import System.Random.Shuffle (shuffleM)
import Text.Read (readMaybe)
import Types

natToInt :: Natural -> Int
natToInt = integerToInt . naturalToInteger

without :: [a] -> Int -> [a]
xs `without` i = take i xs ++ drop (i + 1) xs

showFold :: (Show a, Foldable t) => String -> t a -> String
showFold connector as = helper connector $ toList as
  where
    helper _ [] = ""
    helper c (x : xs) = foldr (\x' a -> a ++ c ++ show x') (show x) xs

sandbox :: GameOpWithCardContext a -> GameOpWithCardContext (Either Player a, GameState)
sandbox op = do
  context <- ask
  currentPlayer <- lift ask
  initialState <- get
  let runReaders = flip runReaderT currentPlayer . flip runReaderT context
  let runMisc = flip runStateT initialState . runExceptT
  liftIO $ runMisc $ runReaders op

checkAll :: OSet (Ex Requirement) -> GameOpWithCardContext Bool
checkAll rs = do
  (res, fstate) <- sandbox $ mapM testRequirement (toList rs) <&> and
  case res of
    Right True -> put fstate >> return True
    _ -> return False

asOpponent :: (MonadReader Player m) => m a -> m a
asOpponent op = do
  p <- ask
  let p' = case p of Player1 -> Player2; Player2 -> Player1
  local (const p') op

asOpponent' :: GameOpWithCardContext a -> GameOpWithCardContext a
asOpponent' op = ask >>= lift . asOpponent . runReaderT op

promoteMonsterSpell :: Card -> Spell -> Card
promoteMonsterSpell m s = m {cardStats = SpellStats s}

selectFromList :: (Show a) => String -> NonEmpty a -> GameOperation (Int, a)
selectFromList _ (option :| []) = return (0, option)
selectFromList prompt options = do
  ask >>= liftIO . putStr . (++ ", ") . show
  liftIO $ putStrLn prompt
  helper $ toList options
  where
    -- (>>) (liftIO $ putStrLn prompt) . liftIO . helper . toList

    printOptions = mapM_ printOption . zip [0 ..]
    helper xs = do
      liftIO $ printOptions xs
      liftIO getLine <&> readMaybe >>= \case
        Just i -> return (i, xs !! i)
        Nothing -> do
          liftIO $ putStrLn "Invalid input, please try again..."
          helper xs
    printOption (i :: Int, s) = putStrLn (show i ++ ": " ++ show s)

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

ifNotCancelled :: (MonadIO m) => Cancelable (i, a) -> ((i, a) -> m ()) -> m ()
ifNotCancelled c f = case c of
  Cancel -> liftIO $ putStrLn "Cancelled."
  Option p -> f p

playerState :: GameOperation PlayerState
playerState = do
  stateGetter <- asks getPlayerState
  gets stateGetter

playerState' :: GameOpWithCardContext PlayerState
playerState' = lift playerState

updatePlayerState :: (PlayerState -> PlayerState) -> GameOperation ()
updatePlayerState f = asks h >>= modify
  where
    h Player1 gs = gs {player1State = f $ player1State gs}
    h Player2 gs = gs {player2State = f $ player2State gs}

updatePlayerState' :: (PlayerState -> PlayerState) -> GameOpWithCardContext ()
updatePlayerState' = lift . updatePlayerState

deckout :: GameOperation a
deckout = ask >>= throwError

draw :: GameOperation ()
draw =
  playerState <&> deck >>= \case
    [] -> ask >>= throwError
    (c : deck) -> do
      updatePlayerState $ \p -> p {deck = deck, hand = c : hand p}
      void $ trigger OnDraw c

shuffleDeck :: GameOperation ()
shuffleDeck = do
  r <- playerState >>= shuffleM . deck
  updatePlayerState $ \p -> p {deck = r}

trigger :: Trigger -> Card -> GameOperation Bool
trigger t = runReaderT activateCard
  where
    activateCard = ask >>= cardElim (`actSpell` t) (`actMonster` t)

actSpell :: Spell -> Trigger -> ReaderT Card GameOperation Bool
actSpell s t =
  if spellTrigger s == t
    then do
      liftIO $ putStrLn ("Attempting to cast " ++ spellName s)
      r <- checkAll (castingConditions s)
      if not r
        then
          liftIO $ putStrLn ("Can't cast " ++ spellName s)
        else mapM_ performEffect $ effects s
      return r
    else return False

actMonster :: Monster -> Trigger -> ReaderT Card GameOperation Bool
actMonster m t
  | isTapped m = do
      liftIO $ putStrLn (monsterName m ++ " is tapped so no spells can trigger.")
      return False
  | isMonsterOnly t = case validMSpells m of
      [] -> do
        liftIO $ putStrLn (monsterName m ++ " has no spells that can be activated in that way.")
        return False
      options ->
        selectFromListCancelable' "Select a monster spell to activate:" options >>= \case
          Cancel -> liftIO $ putStrLn "Canncelled." >> return False
          Option (_, s) -> do
            didCast <- actSpell s t
            when (didCast && t == OnTap) tapThisCard
            return didCast
  | otherwise = do
      spellResults <- mapM (`actSpell` t) $ validMSpells m
      return $ or spellResults
  where
    validMSpells = filter ((t ==) . spellTrigger) . monsterSpells

tapThisCard :: GameOpWithCardContext ()
tapThisCard = do
  cid <- asks cardID
  res <- lift playerState <&> findIndex ((==) cid . cardID) . field
  case res of
    Nothing -> return ()
    Just i -> lift $ updatePlayerState $ \p -> p {field = tapAtI i $ field p}
  where
    tapAtI i cs = take i cs ++ [tap (cs !! i)] ++ drop (i + 1) cs
    tap c = c {cardStats = tapm $ cardStats c}
    tapm (MonsterStats m) = MonsterStats $ m {isTapped = True}
    tapm (SpellStats s) = SpellStats s

printCardsIn :: CardLocation -> GameOperation ()
printCardsIn l = playerState >>= liftIO . putStrLn . showFold "\t" . map cardName . toLens l

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
        ++ map (("\n\t" ++) . show) ss
        ++ [ "\n\tPower ",
             show p,
             if t then "\t[Tapped]" else ""
           ]

instance Show CardStats where
  show (SpellStats s) = show s
  show (MonsterStats s) = show s

instance Show Card where
  show (Card i fs cs) =
    concat $
      [ "(",
        show $ scale (Card i fs cs),
        ") ",
        show cs
      ]
        ++ if null fs
          then []
          else
            [ "\n\t(",
              showFold ", " $ toList fs,
              ")"
            ]
