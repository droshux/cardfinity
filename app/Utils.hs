{-# LANGUAGE LambdaCase #-}
{-# HLINT ignore "Redundant <&>" #-}
{-# OPTIONS_GHC -Wno-missing-signatures #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module Utils
  ( natToInt,
    without,
    showFold,
    whenJust,
    checkAll,
    asOpponent,
    asOpponent',
    selectFromList,
    selectFromList',
    selectFromListCancelable,
    selectFromListCancelable',
    selectFromListNoPlayer,
    selectFromListNoPlayer',
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
    (-=),
    player's,
    opponent's,
    player's',
    SearchType (..),
    toPredicate,
    rarity,
    playCard,
  )
where

import Control.Monad (mfilter, unless, (<=<))
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
import Optics (Lens', over, set, view, (%), (.~), (^.))
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

whenJust :: (Monad m) => (a -> m ()) -> Maybe a -> m ()
whenJust _ Nothing = return ()
whenJust op (Just x) = op x

data SearchType = ForName String | ForFamily String | ForSpell | ForMonster | ForCard deriving (Ord)

instance Eq SearchType where
  (==) (ForName _) (ForName _) = True
  (==) (ForFamily _) (ForFamily _) = True
  (==) ForSpell ForSpell = True
  (==) ForMonster ForMonster = True
  (==) ForCard ForCard = True
  (==) _ _ = False

toPredicate :: SearchType -> Card -> Bool
toPredicate (ForName n) = (n ==) . cardName
toPredicate (ForFamily f) = elem f . (^. cardFamilies)
toPredicate ForCard = const True
toPredicate t = cardElim (const $ t == ForSpell) (const $ t == ForMonster)

instance HasScale SearchType where
  scale t = asks deckContext <&> length . filter (toPredicate t) >>= calcRarity
    where
      calcRarity :: Int -> Scale
      calcRarity x
        | x == 0 = do
            ignore <- asks ignoreSTNotFound
            unless ignore $ throwError $ SearchTypeNotFound $ show t
            return 0
        -- Halfing the number of copies -> increase rarity by 1
        | x == 1 = return 5
        | x == 2 = return 4
        | x >= 3 && x <= 4 = return 3
        | x >= 5 && x <= 8 = return 2
        | x >= 9 && x <= 16 = return 1
        | otherwise = return 0

rarity :: [Card] -> SearchType -> String
rarity dck t = do
  let s = runScale dck t
   in case s of
        Left (SearchTypeNotFound _) -> "Not Found"
        Right 5 -> "Legendary"
        Right 4 -> "Very Rare"
        Right 3 -> "Rare"
        Right 2 -> "Uncommon"
        Right 1 -> "Commmon"
        Right 0 -> "Very Common"
        _ -> "Error"

instance Show SearchType where
  show ForCard = "card"
  show ForSpell = "spell"
  show ForMonster = "monster"
  show (ForName n) = show n
  show (ForFamily f) = show f ++ " card"

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

ifNotCancelled :: (MonadIO m) => Cancelable (i, a) -> ((i, a) -> m ()) -> m ()
ifNotCancelled c f = case c of
  Cancel -> liftIO $ putStrLn "Cancelled."
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
trigger t
  | t == OnDiscard = runReaderT activateIfInGY
  | otherwise = runReaderT activateCard
  where
    activateIfInGY =
      findThisCard >>= \case
        Just (_, Graveyard) -> activateCard
        _ -> liftIO $ do
          putStrLn "Discard effects can only trigger on cards in the graveyard."
          return False
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
  | t == OnAttach = case reverse $ validMSpells m of
      [] -> do
        liftIO $ putStrLn (m ^. monsterName ++ " has no spells that trigger when attached.")
        return False
      (s : _) -> actSpell s t
  | isManual t = case validMSpells m of
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
    isManual OnTap = True
    isManual Infinity = True
    isManual _ = False

playCard :: SearchType -> GameOperation ()
playCard t =
  player's hand <&> filter canPlay . filter (toPredicate t) >>= \case
    [] -> liftIO $ putStrLn ("No " ++ show t ++ " in the hand.")
    cs -> do
      let select = selectFromListCancelable "Choose a card to play: "
      r <- select $ map cardName cs
      ifNotCancelled r (cardElim' playSpell playMonster . (cs !!) . fst)
  where
    canPlay = cardElim ((== OnPlay) . (^. spellTrigger)) (const True)
    -- Find c in the hand and remove it
    fromHand c =
      player's hand <&> findIndex (\c' -> c ^. cardID == c' ^. cardID) >>= \case
        Nothing -> liftIO $ putStrLn ("Error, " ++ cardName c ++ " not in Hand")
        Just i -> hand -= i
    -- Spell: trigger OnPlay, move it to the GY
    -- Triggering OnPlay tests the summoning conditions
    -- If it returns false then the conditions were not met
    playSpell s =
      ask >>= lift . trigger OnPlay >>= \case
        False -> liftIO $ putStrLn ("Cannot play " ++ s ^. spellName)
        True ->
          ask >>= \c -> lift $ do
            graveyard =: c
            fromHand c
    -- Monster: Test summoning conditions, move to field, trigger OnPlay
    playMonster m = do
      success <- checkAll $ m ^. summoningConditions
      if not success
        then liftIO $ putStrLn ("Failed to summon " ++ m ^. monsterName)
        else
          ask >>= \c -> lift $ do
            field =: c
            fromHand c
            void $ trigger OnPlay c

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

printCardsIn :: String -> CardLocation -> GameOperation ()
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
        ++ map (("\n\t" ++) . show) ss
        ++ [ "\n\tPower ",
             show p,
             if t then "\t[Tapped]" else ""
           ]

instance Show CardStats where
  show (SpellStats s) = show s
  show (MonsterStats s) = show s

instance Show Card where
  show (Card _ fs cs) =
    concat $
      show cs
        : if null fs
          then []
          else
            [ "\n\t(",
              showFold ", " $ toList fs,
              ")"
            ]
