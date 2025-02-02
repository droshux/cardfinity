{-# LANGUAGE LambdaCase #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Redundant <&>" #-}

module Utils
  ( natToInt,
    without,
    showFold,
    checkAll,
    asOpponent,
    asOpponent',
    selectFromList,
    selectFromList',
    selectFromListCancelable,
    selectFromListCancelable',
    ifNotCancelled,
    deckout,
    draw,
    shuffleDeck,
    trigger,
    actSpell,
    tapThisCard,
    findThisCard,
    printCardsIn,
    (.=),
    (=:),
    (%=),
    (?=),
    player's,
    opponent's,
    player's',
    (-=),
  )
where

import Control.Monad ((<=<))
import Control.Monad.Except (MonadIO (liftIO), MonadTrans (lift), runExceptT, throwError, void, when)
import Control.Monad.Reader (MonadReader (ask, local), ReaderT (runReaderT), asks)
import Control.Monad.State (MonadState (get, put), StateT (runStateT), gets, modify)
import Data.Bifunctor (second)
import Data.Foldable (Foldable (toList))
import Data.Functor ((<&>))
import Data.List (findIndex)
import Data.List.NonEmpty (NonEmpty ((:|)))
import GHC.Natural (Natural, naturalToInteger)
import GHC.Num (integerToInt)
import Optics (Lens, Lens', over, view, (%), (.~), (^.))
import Optics.State.Operators qualified as OP
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
    helper c (x : xs) = foldr (\x' a -> a ++ c ++ show x') (show x) $ reverse xs

sandbox :: GameOpWithCardContext a -> GameOpWithCardContext (Either Player a, GameState)
sandbox op = do
  context <- ask
  currentPlayer <- lift ask
  initialState <- get
  let runReaders = flip runReaderT currentPlayer . flip runReaderT context
  let runMisc = flip runStateT initialState . runExceptT
  liftIO $ runMisc $ runReaders op

checkAll :: Conditions -> GameOpWithCardContext Bool
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

selectFromList :: (Show a) => String -> NonEmpty a -> GameOperation (Int, a)
selectFromList _ (option :| []) = return (0, option)
selectFromList prompt options = do
  ask >>= liftIO . putStr . (++ ", ") . show
  liftIO $ putStrLn prompt
  helper $ toList options
  where
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

wps :: (Lens GameState GameState a b1 -> t -> GameOperation b2) -> Lens PlayerState PlayerState a b1 -> t -> GameOperation b2
wps f lens x = asks playerLens >>= flip f x . (% lens)

-- Modify operations to depend on the current player

(?=) :: Lens PlayerState PlayerState (Maybe a) (Maybe t) -> t -> GameOperation ()
(?=) = wps (OP.?=)

(.=) :: Lens' PlayerState a -> a -> GameOperation ()
(.=) = wps (OP..=)

(%=) :: Lens PlayerState PlayerState a b -> (a -> b) -> GameOperation ()
(%=) = wps (OP.%=)

(=:) :: Lens' PlayerState [a] -> a -> GameOperation ()
(=:) lns v = asks playerLens >>= modify . flip over (v :) . (% lns)

(-=) :: Lens' PlayerState [a] -> Int -> GameOperation ()
(-=) lns = (%=) lns . flip without

player's :: Lens' PlayerState a -> GameOperation a
player's lens = asks playerLens >>= gets . view . (% lens)

player's' :: Lens' PlayerState a -> GameOpWithCardContext a
player's' = lift . player's

opponent's :: Lens' PlayerState a -> GameOperation a
opponent's = asOpponent . player's

deckout :: GameOperation a
deckout = ask >>= throwError

draw :: GameOperation ()
draw =
  player's deck >>= \case
    [] -> deckout
    (c : cs) -> do
      deck .= cs
      hand =: c
      void $ trigger OnDraw c

shuffleDeck :: GameOperation ()
shuffleDeck = player's deck >>= shuffleM >>= (deck .=)

trigger :: Trigger -> Card -> GameOperation Bool
trigger t = runReaderT activateCard
  where
    activateCard = ask >>= cardElim (`actSpell` t) (`actMonster` t)

actSpell :: Spell -> Trigger -> ReaderT Card GameOperation Bool
actSpell s t =
  if s ^. spellTrigger == t
    then do
      liftIO $ putStrLn ("Attempting to cast " ++ s ^. spellName)
      r <- checkAll (s ^. castingConditions)
      if not r
        then
          liftIO $ putStrLn ("Can't cast " ++ s ^. spellName)
        else mapM_ performEffect $ s ^. effects
      return r
    else return False

actMonster :: Monster -> Trigger -> ReaderT Card GameOperation Bool
actMonster m t
  | m ^. isTapped = do
      liftIO $ putStrLn (m ^. monsterName ++ " is tapped so no spells can trigger.")
      return False
  | isMonsterOnly t = case validMSpells m of
      [] -> do
        liftIO $ putStrLn (m ^. monsterName ++ " has no spells that can be activated in that way.")
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
    validMSpells monster = filter (\s -> s ^. spellTrigger == t) $ monster ^. monsterSpells

tapThisCard :: GameOpWithCardContext ()
tapThisCard = do
  cid <- asks (^. cardID)
  res <- player's' field <&> findIndex (\c -> c ^. cardID == cid)
  case res of
    Nothing -> return ()
    Just i -> lift $ field %= tapAtI i
  where
    tapAtI i cs = take i cs ++ [tap (cs !! i)] ++ drop (i + 1) cs
    tap = monsterStats % isTapped .~ True

findThisCard :: GameOpWithCardContext (Maybe (Int, CardLocation))
findThisCard = mapM findThisIn allCardLocations <&> fmap (second toEnum) . firstIndex 0
  where
    firstIndex _ [] = Nothing
    firstIndex i (Nothing : xs) = firstIndex (i + 1) xs
    firstIndex i ((Just x) : _) = Just (x, i)
    findThisIn loc = do
      cid <- asks (^. cardID)
      player's' (toLens loc) <&> findIndex (\c -> c ^. cardID == cid)

printCardsIn :: CardLocation -> GameOperation ()
printCardsIn l = player's (toLens l) >>= liftIO . putStrLn . showFold "\t" . map cardName

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
