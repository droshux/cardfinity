{-# LANGUAGE LambdaCase #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Redundant <&>" #-}

module AtomActions
  ( testRequirement,
    performEffect,
    draw,
    playCard,
    actSpell,
  )
where

import Atoms
import Control.Monad
import Control.Monad.Except (ExceptT, throwError)
import Control.Monad.RWS (gets, lift, liftIO, put)
import Control.Monad.Reader (MonadReader (ask), ReaderT, asks, runReaderT)
import Data.Foldable (Foldable (..))
import Data.Functor ((<&>))
import Data.List (findIndex)
import Data.List.NonEmpty (NonEmpty ((:|)))
import Data.List.NonEmpty qualified as NonE
import Data.Maybe (fromMaybe, mapMaybe)
import Data.Set.Ordered (OSet)
import GameUtils
import Numeric.Natural (Natural)
import Optics (Ixed (..), (%))
import Optics.Operators ((^.), (^?))
import System.Random.Shuffle (shuffleM)
import Types
import Utils (addInteger, ifEmpty, ifNone, natToInt, try)

testRequirement :: Condition -> GameOpWithCardContext Bool
testRequirement (Destroy d f) = try $ chooseToDestroy d f
testRequirement DiscardSelf = lift $ do
  (c, cs) <- player's deck `ifEmpty` deckout
  deck .= cs
  handleDiscard Discard c
  return True
testRequirement (TakeDamage n isTrue) = takeDamageHelper n isTrue
testRequirement (HealOpponent n) = try $ do
  r <- lift $ asOpponent' $ player's' graveyard <&> length
  when (r < natToInt n) $ throwError ()
  lift $ asOpponent' $ healHelper n
testRequirement (Pop n) = try $ do
  gy <- lift $ player's' graveyard
  when (length gy < natToInt n) $ throwError ()
  lift $ lift $ graveyard %= drop (natToInt n)
testRequirement (YouMay cond) = do
  let prompt = "Would you like to " ++ show cond
  r <- selectFromList' prompt ("Continue" :| ["Cancel Spell"])
  if fst r == 0 then testRequirement cond else return False
testRequirement (Choose rs) =
  let prompt = "Choose one of the following:"
   in lift (selectFromList prompt rs) >>= testRequirement . snd

performEffect :: Effect -> GameOpWithCardContext ()
performEffect (DestroyEnemy d f) = asOpponent' $ destroyForced d f
performEffect DiscardEnemy = void $ asOpponent' $ testRequirement DiscardSelf
performEffect (DealDamage n isTrue) = void $ asOpponent' $ takeDamageHelper n isTrue
performEffect (Heal n) = healHelper n
performEffect DECKOUT = lift deckout
performEffect (Draw n) = replicateM_ (natToInt n) (lift draw)
performEffect (Peek n) = player's' deck >>= liftIO . mapM_ print . take (natToInt n)
performEffect (Scry n) = asOpponent' $ performEffect $ Peek n
performEffect (Optional e) = do
  let prompt = "Would you like to " ++ show e
  r <- selectFromList' prompt ("Yes" :| ["No"])
  when (fst r == 0) $ performEffect e
performEffect (ChooseEffect es) = lift (selectFromList "Choose one of the following:" es) >>= performEffect . snd
performEffect (Attack piercing) = void $ try $ do
  ft <- gets (^. isFirstTurn)
  when ft $ do
    liftIO $ putStrLn "You cannot attack on the first turn!"
    throwError ()
  location <- lift findThisCard <&> fmap snd
  when (location == Just Field) $ do
    liftIO $ putStrLn "Monsters can only attack from the field!"
    throwError ()
  this <- ask >>= cardElim (const $ throwError ()) return
  let validTarget c = fromMaybe False $ c ^? monsterStats % isTapped
  let targets = opponent's field <&> filter validTarget
  (t, ts) <- ifEmpty (lift $ lift targets) $ do
    liftIO $ putStrLn "Attacking Directly!"
    lift $ performEffect $ DealDamage (this ^. combatPower) False
    throwError ()
  let prompt = "Select the monster to attack:"
  (i, _) <- lift $ selectFromList' prompt $ NonE.map cardName (t :| ts)
  let mbTarget = (t : ts) !! i ^? monsterStats
  target <- ifNone (return mbTarget) $ throwError ()
  lift $ do
    let delta = this ^. combatPower - target ^. combatPower
    when (delta > 0 && piercing) $ performEffect $ DealDamage delta False
    loser <- if delta > 0 then return $ (t : ts) !! i else ask
    lift $ do
      void $ trigger OnDefeat loser
      asOpponent (field -= i)
      handleDiscard Discard loser
performEffect (Play t) = lift $ playCard t
performEffect (Search (SearchFor t)) = void $ try $ do
  let options = player's' deck <&> filter (toPredicate t)
  (cfst, crst) <- ifEmpty (lift options) $ do
    liftIO $ putStrLn ("No " ++ show t ++ "s in the deck.")
    throwError ()
  let prompt = "Select card to draw:"
  (j, _) <- lift $ selectFromList' prompt $ NonE.map cardName $ cfst :| crst
  ids <- lift $ options <&> map (^. cardID)
  let valid c = c ^. cardID == ids !! j
  let find = player's' deck <&> findIndex valid
  i <- ifNone (lift find) $ do
    liftIO $ putStrLn ("No " ++ show t ++ "s in the deck.")
    throwError ()
  lift $ lift $ do
    found <- player's deck <&> (!! i)
    hand =: found
    deck -= i
    shuffleDeck
    void $ trigger OnDraw found
performEffect (Search (DrillFor t)) = do
  (c, cs) <- lift $ ifEmpty (player's deck) deckout
  if toPredicate t c
    then lift $ do
      deck .= cs
      hand =: c
      void $ trigger OnDraw c
    else do
      lift $ deck .= cs
      lift $ graveyard =: c
      performEffect $ Search (DrillFor t)
performEffect (Attach t) = void $ try $ do
  let valid = player's' hand <&> mapMaybe (^? spellStats) . filter (toPredicate t)
  (sfst, srst) <- ifEmpty (lift valid) $ do
    liftIO $ putStrLn "There are no spell cards in your hand."
    throwError ()
  let prompt = "Select a spell to attach:"
  (i, s) <- lift $ selectFromList' prompt (sfst :| srst)
  (j, loc) <- ifNone (lift findThisCard) $ do
    liftIO $ putStrLn ("Error, " ++ s ^. spellName ++ " cannot be found!")
    throwError ()
  myName <- asks cardName
  liftIO $ putStrLn ("Attaching " ++ s ^. spellName ++ " to " ++ myName)
  lift $ lift $ do
    hand -= i
    toLens loc % ix j % monsterStats % monsterSpells %= (s :)
performEffect (Buff by True) = do
  this <- lensThisCard
  lift $ this % monsterStats % combatPower %= addInteger by
performEffect (Buff by False) = void $ try $ do
  (cfst, crst) <- ifEmpty (lift $ player's' field) $ do
    liftIO $ putStrLn "Could not find another card on the field."
    throwError ()
  let prompt = "Select a card to alter:"
  lift $ do
    (i, _) <- selectFromList' prompt $ NonE.map cardName (cfst :| crst)
    lift $ field % ix i % monsterStats % combatPower %= addInteger by
performEffect (AsEffect cond) = void $ testRequirement cond

handleDiscard :: DestroyType -> Card -> GameOperation ()
handleDiscard d c = unless (d == Banish) $ do
  graveyard =: c
  void $ trigger OnDiscard c

chooseToDestroy :: DestroyType -> FindCards -> ExceptT () GameOpWithCardContext ()
chooseToDestroy d f
  | getCount f == 0 = return ()
  | otherwise = do
      cid <- asks (^. cardID)
      let validTarget c = toPredicate (getSearchType f) c && c ^. cardID /= cid
      let targets = player's' (getLocation f) <&> filter validTarget
      (cfst, crst) <- ifEmpty (lift targets) $ do
        liftIO $ do
          putStr "Could not find enough "
          putStr $ show (getSearchType f)
          putStrLn $ case f of
            FindCardsField _ _ -> "s on the Field."
            FindCardsHand _ _ -> "s in the Hand."
        throwError ()
      let prompt = "Select a card to " ++ show d ++ ":"
      (j, _) <- lift $ selectFromList' prompt $ NonE.map cardName (cfst :| crst)
      let c = (cfst : crst) !! j
      liftIO $ putStrLn (show d ++ "ing " ++ cardName c ++ " from the " ++ show f)
      let mbIndex = player's' (getLocation f) <&> findIndex (\c' -> c' ^. cardID == c ^. cardID)
      i <- ifNone (lift mbIndex) $ do
        liftIO $ putStrLn ("Error, " ++ cardName c ++ " not in " ++ show f)
        throwError ()
      lift $ lift $ do
        getLocation f -= i
        handleDiscard d c
      moveOn
  where
    moveOn = chooseToDestroy d $ case f of
      FindCardsHand n t -> FindCardsHand (n - 1) t
      FindCardsField n t -> FindCardsField (n - 1) t

destroyForced :: DestroyType -> FindCards -> GameOpWithCardContext ()
destroyForced d (FindCardsHand n st)
  | n == 0 = return ()
  | otherwise = void $ try $ do
      let shuffledOptions = player's' hand <&> filter (toPredicate st) >>= shuffleM
      (c, _) <- ifEmpty (lift shuffledOptions) $ do
        liftIO $ do
          putStrLn "Couldn't find enough "
          putStr $ show st
          putStr "s in the hand."
        throwError ()
      lift $ do
        lift $ handleDiscard d c
        destroyForced d $ FindCardsHand (n - 1) st
destroyForced d (FindCardsField n st)
  | n == 0 = return ()
  | otherwise = void $ try $ do
      let getTargets = player's' field <&> filter (toPredicate st)
      (c, cs) <- ifEmpty (lift getTargets) $ do
        liftIO $ do
          putStrLn "Couldn't find enough "
          putStr $ show st
          putStr "s on the field."
        throwError ()
      lift $ do
        this <- lift ask
        (i, _) <- selectFromListNoPlayer' (prompt this) $ NonE.map cardName (c :| cs)
        lift $ field -= i
        lift $ handleDiscard d $ (c : cs) !! i
        destroyForced d $ FindCardsField (n - 1) st
  where
    prompt cp =
      concat
        [ show (otherPlayer cp),
          ": choose a ",
          show st,
          " from ",
          show cp,
          "'s Field"
        ]

takeDamageHelper :: Natural -> Bool -> GameOpWithCardContext Bool
takeDamageHelper 0 _ = return True
takeDamageHelper n isTrue =
  player's' deck >>= \case
    [] -> lift deckout
    (c : dck) -> do
      lift $ deck .= dck
      lift $ unless isTrue $ graveyard =: c
      takeDamageHelper (n - 1) isTrue

healHelper :: Natural -> GameOpWithCardContext ()
healHelper 0 = return ()
healHelper n =
  player's' graveyard >>= \case
    [] -> liftIO $ putStrLn "No more cards in the Graveyard."
    (c : cs) -> do
      lift $ do
        deck =: c
        graveyard .= cs
      performEffect $ Heal (n - 1)

checkAll :: OSet Condition -> GameOpWithCardContext Bool
checkAll rs = do
  (res, fstate) <- sandbox $ mapM testRequirement (toList rs) <&> and
  case res of
    Right True -> put fstate >> return True
    _ -> return False

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
actSpell s t = try $ do
  unless (s ^. spellTrigger == t) $ throwError ()
  liftIO $ putStrLn ("Attempting to cast " ++ s ^. spellName)
  r <- lift $ checkAll (s ^. castingConditions)
  unless r $ do
    liftIO $ putStrLn ("Can't cast " ++ s ^. spellName)
    throwError ()
  lift $ mapM_ performEffect $ s ^. effects

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
playCard t = void $ try $ do
  let getOptions = player's hand <&> filter canPlay . filter (toPredicate t)
  (c, cs') <- ifEmpty getOptions $ do
    liftIO $ putStrLn ("No " ++ show t ++ " in the hand.")
    throwError ()
  let cs = c : cs'
  let select = selectFromListCancelable "Choose a card to play: "
  r <- lift $ select $ map cardName cs
  let get = (cs !!) . fst
  lift $ ifNotCancelled r (cardElim' playSpell playMonster . get)
  where
    canPlay = cardElim ((== OnPlay) . (^. spellTrigger)) (const True)

    -- Find c in the hand and remove it
    fromHand c = void $ try $ do
      let getIndex = player's hand <&> findIndex (\c' -> c ^. cardID == c' ^. cardID)
      i <- ifNone (lift getIndex) $ do
        liftIO $ putStrLn ("Error, " ++ cardName c ++ " not in Hand")
        throwError ()
      hand -= i

    -- Spell: trigger OnPlay, move it to the GY
    -- Triggering OnPlay tests the summoning conditions
    -- If it returns false then the conditions were not met
    playSpell s = void $ try $ do
      successfulCast <- ask >>= lift . lift . trigger OnPlay
      unless successfulCast $ do
        liftIO $ putStrLn ("Cannot play " ++ s ^. spellName)
        throwError ()
      c <- ask
      lift $ lift $ do
        graveyard =: c
        fromHand c

    -- Monster: Test summoning conditions, move to field, trigger OnPlay
    playMonster m = void $ try $ do
      success <- lift $ checkAll $ m ^. summoningConditions
      unless success $ do
        liftIO $ putStrLn ("Failed to summon " ++ m ^. monsterName)
        throwError ()
      c <- ask
      lift $ lift $ do
        field =: c
        fromHand c
        void $ trigger OnPlay c

draw :: GameOperation ()
draw = do
  (c, cs) <- player's deck `ifEmpty` deckout
  deck .= cs
  hand =: c
  void $ trigger OnDraw c
