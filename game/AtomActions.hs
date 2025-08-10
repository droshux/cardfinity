{-# LANGUAGE LambdaCase #-}

module AtomActions where

import Atoms (Condition (..), Effect (..))
import Control.Monad.Except (runExceptT, throwError)
import Data.Either (isRight)
import Data.Foldable (Foldable (length))
import Types (GameOpWithCardContext, deck, graveyard)
import Utils (asOpponent', deckout, natToInt, player's, player's')

testRequirement :: Condition -> GameOpWithCardContext Bool
testRequirement (Destroy d f) = chooseToDestroy d f
testRequirement DiscardSelf =
  lift $
    player's deck >>= \case
      [] -> deckout
      (c : cs) -> deck .= cs >> handleDiscard Discard c
testRequirement (TakeDamage n isTrue) = takeDamageHelper n isTrue
testRequirement (HealOpponent n) = isRight $ runExceptT $ do
  r <- asOpponent' $ player's' graveyard <&> length
  when (r < natToInt n) $ throwError ()
  asOpponent' $ healHelper n
testRequirement (Pop n) = isRight $ runExceptT $ lift $ do
  gy <- player's graveyard
  when (length gy < natToInt n) $ throwError ()
  graveyard %= drop (natToInt n)
testRequirement (YouMay cond) = do
  let prompt = "Would you like to " ++ displayRequirement req
  r <- selectFromList' prompt ("Continue" :| ["Cancel Spell"])
  if fst r == 0 then testRequirement req else return False
testRequirement (Choose rs) = lift (selectFromList "Choose one of the following:" rs) >>= testRequirement . snd

performEffect :: Effect -> GameOpWithCardContext ()
performEffect (DestroyEnemy d f) = asOpponent' $ destroyForced d f
performEffect (DealDamage n isTrue) = void $ asOpponent' $ takeDamageHelper n isTrue
performEffect (Heal n) = healHelper n
performEffect DECKOUT = lift deckout
performEffect (Draw n) = replicateM_ (natToInt n) (lift draw)
performEffect (Peek n) = player's' deck >>= liftIO . mapM_ print . take (natToInt n)
performEffect (Scry n) = asOpponent' $ performEffect $ Peek n
performEffect (Optional e) = do
  let prompt = "Would you like to " ++ displayEffect e
  r <- selectFromList' prompt ("Yes" :| ["No"])
  when (fst r == 0) $ performEffect e
performEffect (ChooseEffect es) = lift (selectFromList "Choose one of the following:" es) >>= performEffect . snd
performEffect (Attack piercing) =
  let attackDirectly m = do
        liftIO $ putStrLn "Attacking Directly!"
        lift $ asOpponent $ takeBattleDamage m
      attackable c = case c ^. cardStats of
        SpellStats _ -> False
        MonsterStats m -> not $ m ^. isTapped
      attackIndirect m =
        let power = natToInt $ m ^. combatPower
         in lift (opponent's field) <&> filter attackable >>= \case
              [] -> attackDirectly power
              (efst : erst) -> do
                (i, _) <- selectFromList' "Select the monster to attack:" $ NonE.map cardName (efst :| erst)
                let target = (efst : erst) !! i
                let deltaPower = power - maybe 0 natToInt (target ^? monsterStats % combatPower)
                let iWon = deltaPower >= 0
                when (iWon && piercing) $ lift $ asOpponent $ takeBattleDamage $ max 0 deltaPower
                if iWon then defeatTarget target else defeatThis
                when iWon $ ask >>= lift . void . trigger OnVictory
      takeBattleDamage n = do
        dtop <- player's deck <&> take n
        graveyard %= (++ dtop)
        deck %= drop n
        dead <- player's deck <&> null
        when dead deckout
      defeatTarget = lift . asOpponent . runReaderT defeatThis
      defeatThis = do
        -- Get the location of this card and send it to the graveyard
        cid <- asks (^. cardID)
        mbIndex <- player's' field <&> findIndex (\c -> c ^. cardID == cid)
        case mbIndex of
          Nothing -> liftIO (putStrLn "Error: Cannot find this card on the field.")
          Just i -> lift $ do
            -- Send to the graveyard
            c <- player's field <&> (!! i)
            graveyard =: c
            -- Remove from field
            field -= i

        ask >>= lift . void . trigger OnDefeat
        ask >>= lift . void . trigger OnDiscard
   in do
        ft <- gets (^. isFirstTurn)
        if ft
          then liftIO $ putStrLn "You cannot attack on the first turn."
          else do
            l <- findThisCard <&> fmap snd -- Only monsters on the field can attack.
            if l == Just Field
              then ask >>= cardElim (const $ return ()) attackIndirect
              else liftIO $ putStrLn "Only monsters on the field can attack."
performEffect (Play t) = lift $ playCard t
performEffect (Search (SearchFor t)) =
  let options = player's' deck <&> filter (toPredicate t)
   in options >>= \case
        [] -> liftIO $ putStrLn ("No " ++ show t ++ "s in the deck.")
        (cfst : crst) -> do
          ids <- options <&> map (^. cardID)
          (i', _) <- selectFromList' "Select card to draw:" $ NonE.map cardName $ cfst :| crst
          player's' deck <&> findIndex (\c -> c ^. cardID == ids !! i') >>= \case
            Nothing -> liftIO $ putStrLn $ cardName ((cfst : crst) !! i') ++ " not found in deck?!"
            Just i -> lift $ do
              c <- player's deck <&> (!! i)
              hand =: c
              deck -= i
              shuffleDeck
              void $ trigger OnDraw c
performEffect (Search (DrillFor t)) =
  player's' deck >>= \case
    [] -> lift deckout
    (c : cs) ->
      if not $ toPredicate t c
        then do
          lift $ deck .= cs
          lift $ graveyard =: c
          performEffect $ Search (DrillFor t)
        else lift $ do
          deck .= cs
          hand =: c
          void $ trigger OnDraw c
performEffect (Attach t) =
  let updateCard (i, loc) f = do
        let c = toLens loc % ix i
        c %= f
        asks playerLens >>= gets . preview . (% c) >>= whenJust (void . trigger OnAttach)
   in player's' hand <&> mapMaybe (^? spellStats) . filter (toPredicate t) >>= \case
        [] -> liftIO $ putStrLn "There are no spell cards in your hand."
        (sfst : srst) -> do
          (i, s) <- selectFromList' "Select a spell to attach:" (sfst :| srst)
          findThisCard >>= \case
            Nothing -> liftIO $ putStrLn ("Error, " ++ s ^. spellName ++ " cannot be found!")
            Just p -> lift $ do
              liftIO $ putStr ("Attaching " ++ s ^. spellName ++ " to ")
              hand -= i
              updateCard p $ monsterStats % monsterSpells %~ (s :)
performEffect (Buff by forItself) =
  let alterMy n = do
        cid <- asks (^. cardID)
        player's' field <&> findIndex (\c -> cid == c ^. cardID) >>= \case
          Nothing -> do
            name <- asks cardName
            liftIO $ putStrLn ("Error " ++ name ++ " not found on ")
          Just i -> lift $ do
            addPower i n
      alterTarget n = do
        player's' field >>= \case
          [] -> liftIO $ putStrLn "Could not find another card on the field."
          (cfst : crst) -> do
            let names = cardName cfst :| map cardName crst
            (i, _) <- selectFromList' "Select a card to alter:" names
            lift $ addPower i n
      addPower i n = field % ix i %= (monsterStats % combatPower %~ (+ fromIntegral n))
   in (if forItself then alterMy else alterTarget) by
performEffect (AsEffect cond) = void $ testRequirement cond

handleDiscard d c = unless (d == Banish) $ do
  graveyard =: c
  void $ trigger OnDiscard c

chooseToDestroy :: DestroyType -> FindCards -> GameOpWithCardContext Bool
chooseToDestroy d f
  | getCount f == 0 = return True
  | otherwise = do
      cid <- asks (^. cardID)
      let validTarget c = toPredicate (getSearchType f) c && c ^. cardID /= cid
      player's' (getLocation f) <&> filter validTarget >>= \case
        [] -> liftIO $ do
          putStr "Could not find enough "
          putStr $ show (getSearchType f)
          putStrLn $ case f of
            FindCardsField _ _ -> "s on the Field."
            FindCardsHand _ _ -> "s in the Hand."
          return False
        (cfst : crst) -> do
          let names = cardName cfst :| map cardName crst
          (i, _) <- selectFromList' ("Select a card to " ++ show d ++ ":") names
          let c = (cfst : crst) !! i
          liftIO $ putStrLn (show d ++ "ing " ++ cardName c ++ " from the " ++ show f)
          player's' (getLocation f) <&> findIndex (\c' -> c' ^. cardID == c ^. cardID) >>= \case
            Nothing -> liftIO $ putStrLn ("Error, " ++ cardName c ++ " not in " ++ show f)
            Just j -> lift $ do
              getLocation f -= j
              handleDiscard d c
          moveOn
  where
    moveOn = chooseToDestroy d $ case f of
      FindCardsHand n t -> FindCardsHand (n - 1) t
      FindCardsField n t -> FindCardsField (n - 1) t

destroyForced :: DestroyType -> FindCards -> GameOpWithCardContext ()
destroyForced d (FindCardsHand n st)
  | n == 0 = return ()
  | otherwise =
      player's' hand <&> filter (toPredicate st) >>= shuffleM >>= \case
        [] -> liftIO $ do
          putStrLn "Couldn't find enough "
          putStr $ show st
          putStr "s in the hand."
        (c : _) -> do
          lift $ handleDiscard d c
          destroyForced d $ FindCardsHand (n - 1) st
destroyForced d (FindCardsField n st)
  | n == 0 = return ()
  | otherwise =
      player's' field <&> filter (toPredicate st) >>= \case
        [] -> liftIO $ do
          putStrLn "Couldn't find enough "
          putStr $ show st
          putStr "s on the field."
        (cfst : crst) -> do
          cp <- lift ask
          (i, _) <- selectFromListNoPlayer' (prompt cp) $ cardName cfst :| map cardName crst
          lift $ field -= i
          lift $ handleDiscard d $ (cfst : crst) !! i
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
      performEffect $ heal (n - 1)
