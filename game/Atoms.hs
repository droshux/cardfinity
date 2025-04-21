{-# LANGUAGE LambdaCase #-}
{-# HLINT ignore "Redundant <&>" #-}
{-# OPTIONS_GHC -Wno-missing-signatures #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

module Atoms
  ( FindCards (..),
    -- ifCards,
    DestroyType (..),
    destroyCards,
    destroyTheirCards,
    takeDamage,
    dealDamage,
    deckoutEffect,
    drawEffect,
    peek,
    scry,
    popGraveyard,
    choose,
    attack,
    discardDeck,
    discardTheirDeck,
    heal,
    healOpponent,
    attach,
    youMay,
    search,
    SearchMethod (..),
    asEffect,
    alterPower,
    reqYouMay,
    -- destroyThis,
    playCardEffect,
  )
where

import Control.Monad (void)
import Control.Monad.Except (MonadIO (liftIO), MonadTrans (lift), replicateM_, when)
import Control.Monad.RWS (MonadReader (ask), asks, gets, unless)
import Control.Monad.Reader (ReaderT (runReaderT))
import Data.Default (def)
import Data.Functor ((<&>))
import Data.List (findIndex, stripPrefix)
import Data.List.NonEmpty (NonEmpty (..))
import Data.List.NonEmpty qualified as NonE
import Data.Maybe (mapMaybe)
import GHC.Natural (Natural)
import Optics (Ixed (ix), preview)
import Optics.Operators ((%~), (^.), (^?))
import Optics.Optic ((%))
import Types
import Utils

data FindCards = FindCardsField Natural SearchType | FindCardsHand Natural SearchType deriving (Eq)

getCount :: FindCards -> Natural
getCount (FindCardsField n _) = n
getCount (FindCardsHand n _) = n

getLocation (FindCardsField _ _) = field
getLocation (FindCardsHand _ _) = hand

getSearchType :: FindCards -> SearchType
getSearchType (FindCardsHand _ t) = t
getSearchType (FindCardsField _ t) = t

isField :: FindCards -> Bool
isField (FindCardsField _ _) = True
isField (FindCardsHand _ _) = False

instance Show FindCards where
  show f = case f of
    FindCardsHand n t -> part1 n t ++ "in the hand"
    FindCardsField n t -> part1 n t ++ "on the field"
    where
      part1 n t = concat [show n, " ", show t, if n == 1 then " " else "s "]

{-ifCards :: FindCards -> Requirement
ifCards f =
  Requirement
    { testRequirement =
        player's' (getLocation f) <&> (>= natToInt (getCount f)) . length . h f,
      requirementScale = (if isField f then 4 else 1) * (-natToInt (getCount f)),
      monsterOnlyRequirement = False,
      displayRequirement = "If there " ++ isare f ++ "at least " ++ show f
    }
  where
    isare i = if getCount i == 1 then " is " else " are "
    h f' = filter (toPredicate $ getSearchType f')
-}

data DestroyType = Discard | Banish deriving (Eq, Ord, Show)

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
          -- playerState' <&> findIndex ((== _cardID c) . _cardID) . toLens other >>= \case
          player's' (getLocation f) <&> findIndex (\c' -> c' ^. cardID == c ^. cardID) >>= \case
            Nothing -> liftIO $ putStrLn ("Error, " ++ cardName c ++ " not in " ++ show f)
            Just j -> lift $ do
              getLocation f -= j
              handleDiscard c
          moveOn
  where
    moveOn = chooseToDestroy d $ case f of
      FindCardsHand n t -> FindCardsHand (n - 1) t
      FindCardsField n t -> FindCardsField (n - 1) t
    handleDiscard c = unless (d == Banish) $ do
      graveyard =: c
      void $ trigger OnDiscard c

destroyForced :: DestroyType -> FindCards -> GameOpWithCardContext ()
destroyForced d f
  | getCount f == 0 = return ()
  | otherwise =
      player's' (getLocation f) <&> filter (toPredicate $ getSearchType f) >>= \case
        [] -> liftIO $ do
          putStrLn "Couldn't find enough "
          putStr $ show $ getSearchType f
          putStrLn "s in the hand."
        (cfst : crst) -> do
          cp <- lift ask
          (i, _) <- selectFromList' (prompt cp) $ cardName cfst :| map cardName crst
          lift $ getLocation f -= i
          lift $ handleDiscard $ (cfst : crst) !! i
          moveOn
  where
    handleDiscard c = unless (d == Banish) $ do
      graveyard =: c
      void $ trigger OnDiscard c
    moveOn = destroyForced d $ case f of
      FindCardsHand n t -> FindCardsHand (n - 1) t
      FindCardsField n t -> FindCardsField (n - 1) t
    prompt cp =
      concat
        [ show (otherPlayer cp),
          ": choose a ",
          show (getSearchType f),
          " from ",
          show cp,
          "'s ",
          if isField f then "Field" else "Hand"
        ]

-- Destroy your own cards as a Requirement
destroyCards :: DestroyType -> FindCards -> Requirement
destroyCards d f =
  Requirement
    { testRequirement = chooseToDestroy d f,
      requirementScale = return $ natToInt (getCount f) * (-multiplier),
      monsterOnlyRequirement = False,
      displayRequirement = show d ++ " " ++ show f
    }
  where
    multiplier =
      (if isField f then 15 else 10)
        + (if d == Banish then 2 else 0)
        + (if getSearchType f == ForSpell then 2 else 0)

-- Destroy their cards as an effect
destroyTheirCards :: DestroyType -> FindCards -> Effect
destroyTheirCards d f =
  Effect
    { performEffect = asOpponent' $ destroyForced d f,
      monsterOnlyEffect = False,
      effectScale = scale inverted <&> (\x -> -x),
      displayEffect = let destOurs = show inverted in replaceLast " the " " the enemy " destOurs
    }
  where
    inverted = destroyCards d f
    replaceLast b a s = reverse $ replaceFirst (reverse b) (reverse a) (reverse s)
    replaceFirst _ _ [] = []
    replaceFirst b a (c : cs) = case stripPrefix b (c : cs) of
      Just cs' -> a ++ cs'
      Nothing -> c : replaceFirst b a cs

-- Only destoys the top card of the deck
discardDeck :: Requirement
discardDeck =
  Requirement
    { testRequirement =
        lift $
          player's deck >>= \case
            [] -> deckout
            (c : cs) -> do
              deck .= cs
              graveyard =: c
              void $ trigger OnDiscard c
              return True,
      requirementScale = return $ -4,
      monsterOnlyRequirement = False,
      displayRequirement = "Discard the top card of the deck"
    }

discardTheirDeck :: Effect
discardTheirDeck =
  Effect
    { performEffect = void $ asOpponent' $ testRequirement discardDeck,
      monsterOnlyEffect = False,
      effectScale = requirementScale discardDeck <&> (\x -> -x),
      displayEffect = "Discard the top card of the opponent's deck"
    }

takeDamage :: Natural -> Bool -> Requirement
takeDamage n isTrue =
  Requirement
    { testRequirement = takeDamageHelper n isTrue,
      requirementScale = return $ natToInt n * (if isTrue then -7 else -5),
      monsterOnlyRequirement = False,
      displayRequirement = concat ["Take ", show n, if isTrue then " True" else "", " damage"]
    }

dealDamage :: Natural -> Bool -> Effect
dealDamage n isTrue =
  Effect
    { performEffect = void $ asOpponent' $ takeDamageHelper n isTrue,
      monsterOnlyEffect = False,
      effectScale = scale (takeDamage n isTrue) <&> (\x -> -x),
      displayEffect = concat ["Deal ", show n, if isTrue then " True" else "", " damage"]
    }

takeDamageHelper :: Natural -> Bool -> GameOpWithCardContext Bool
takeDamageHelper 0 _ = return True
takeDamageHelper n isTrue =
  player's' deck >>= \case
    [] -> lift deckout
    (c : dck) -> do
      lift $ deck .= dck
      lift $ unless isTrue $ graveyard =: c
      takeDamageHelper (n - 1) isTrue

heal :: Natural -> Effect
heal n =
  Effect
    { performEffect = healHelper n,
      monsterOnlyEffect = False,
      effectScale = return $ 7 * natToInt n,
      displayEffect = "Heal " ++ show n ++ " damage"
    }

healOpponent :: Natural -> Requirement
healOpponent n =
  Requirement
    { testRequirement = do
        r <- asOpponent' $ player's' graveyard <&> length
        if r < natToInt n
          then return False
          else asOpponent' $ healHelper n >> return True,
      requirementScale = scale (heal n) <&> (\x -> -x),
      monsterOnlyRequirement = False,
      displayRequirement = "Heal the opponent for " ++ show n ++ " damage"
    }

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

deckoutEffect :: Effect
deckoutEffect =
  def
    { displayEffect = "DECKOUT",
      effectScale = return $ -punishment,
      performEffect = lift deckout
    }

drawEffect :: Natural -> Effect
drawEffect n =
  def
    { effectScale = return $ natToInt n * 10,
      displayEffect = "Draw " ++ show n ++ " card" ++ if n == 1 then " " else "s",
      performEffect = replicateM_ (natToInt n) (lift draw)
    }

peek :: Natural -> Effect
peek n =
  def
    { effectScale = return $ 2 ^ n,
      performEffect = player's' deck >>= liftIO . mapM_ print . take (natToInt n),
      displayEffect =
        concat
          [ "See the top ",
            if n == 1 then "" else show n ++ " ",
            "card",
            if n == 1 then "" else "s",
            " of the deck"
          ]
    }

scry :: Natural -> Effect
scry n =
  let p = peek n
   in p
        { performEffect = asOpponent' $ performEffect p,
          displayEffect =
            concat
              [ "See the top ",
                if n == 1 then "" else show n ++ " ",
                "card",
                if n == 1 then "" else "s",
                " of the opponent's deck"
              ]
        }

popGraveyard :: Natural -> Requirement
popGraveyard n =
  def
    { requirementScale = return $ -(2 * natToInt n),
      displayRequirement =
        concat
          [ "Banish the top ",
            if n == 1 then "" else show n ++ " ",
            "card",
            if n == 1 then "" else "s",
            " of the graveyard"
          ],
      testRequirement = do
        gy <- player's' graveyard
        if length gy < natToInt n
          then return False
          else do
            lift $ graveyard %= drop (natToInt n)
            return True
    }

choose :: NonEmpty Effect -> Effect
choose es =
  def
    { displayEffect = "Choose one of " ++ showFold " or " es,
      -- effectScale = return $ maximum (mapM scale $ NonE.toList es),
      effectScale = mapM scale (NonE.toList es) <&> maximum,
      performEffect = do
        (_, c) <- lift $ selectFromList "Choose one of the following:" es
        performEffect c
    }

attack :: Bool -> Effect
attack piercing =
  Effect
    { displayEffect =
        "Attack with this monster"
          ++ if piercing then " dealing piercing damage" else "",
      effectScale = return $ if piercing then 20 else 10,
      monsterOnlyEffect = True,
      performEffect = do
        ft <- gets (^. isFirstTurn)
        if ft
          then liftIO $ putStrLn "You cannot attack on the first turn."
          else do
            l <- findThisCard <&> fmap snd -- Only monsters on the field can attack.
            if l == Just Field
              then ask >>= cardElim (const $ return ()) attackIndirect
              else liftIO $ putStrLn "Only monsters on the field can attack."
    }
  where
    attackDirectly m = do
      liftIO $ putStrLn "Attacking Directly!"
      lift $ asOpponent $ takeBattleDamage m
    attackIndirect m =
      let power = natToInt $ m ^. combatPower
       in lift (opponent's field) >>= \case
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

data SearchMethod = SearchFor SearchType | DrillFor SearchType

search :: SearchMethod -> Effect
search (SearchFor t) =
  def
    { effectScale = return $ if t == ForSpell then 25 else 30,
      displayEffect = "Search the deck for a " ++ show t,
      performEffect =
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
    }
search (DrillFor t) =
  def
    { effectScale = return $ if t == ForSpell then 10 else 15,
      displayEffect = "Drill the deck for a " ++ show t,
      performEffect =
        player's' deck >>= \case
          [] -> lift deckout
          (c : cs) ->
            if not $ toPredicate t c
              then do
                lift $ deck .= cs
                lift $ graveyard =: c
                performEffect $ search (DrillFor t)
              else lift $ do
                deck .= cs
                hand =: c
                void $ trigger OnDraw c
    }

attach :: SearchType -> Effect
attach t =
  Effect
    { performEffect =
        player's' hand <&> mapMaybe (^? spellStats) . filter (toPredicate t) >>= \case
          [] -> liftIO $ putStrLn "There are no spell cards in your hand."
          (sfst : srst) -> do
            (i, s) <- selectFromList' "Select a spell to attach:" (sfst :| srst)
            findThisCard >>= \case
              Nothing -> liftIO $ putStrLn ("Error, " ++ s ^. spellName ++ " cannot be found!")
              Just p -> lift $ do
                liftIO $ putStr ("Attaching " ++ s ^. spellName ++ " to ")
                hand -= i
                updateCard p $ monsterStats % monsterSpells %~ (s :),
      monsterOnlyEffect = True,
      effectScale = return 5,
      displayEffect = "Attach a " ++ show t ++ " from your hand to this card"
    }
  where
    updateCard (i, loc) f = do
      let c = toLens loc % ix i
      c %= f
      asks playerLens >>= gets . preview . (% c) >>= whenJust (void . trigger OnAttach)

youMay :: Effect -> Effect
youMay e =
  Effect
    { performEffect = do
        let prompt = "Would you like to " ++ displayEffect e
        r <- selectFromList' prompt ("Yes" :| ["No"])
        when (fst r == 0) $ performEffect e,
      monsterOnlyEffect = False,
      effectScale = scale e <&> max (-punishment),
      displayEffect = "You may " ++ show e
    }

asEffect :: Requirement -> Effect
asEffect r =
  Effect
    { performEffect = void $ testRequirement r,
      monsterOnlyEffect = False,
      effectScale = scale r <&> max (-punishment),
      displayEffect = displayRequirement r
    }

alterPower :: Integer -> Bool -> Effect
alterPower by forItself =
  Effect
    { performEffect = (if forItself then alterMy else alterTarget) by,
      monsterOnlyEffect = forItself,
      effectScale = return $ max (-punishment) $ if forItself then 2 * fromIntegral by else 3 * fromIntegral by,
      displayEffect =
        (++ show by) $
          if forItself
            then "Increase this card's power by "
            else "Increase a card on the Field's power by "
    }
  where
    alterMy n = do
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

reqYouMay :: Requirement -> Requirement
reqYouMay req =
  Requirement
    { testRequirement = do
        let prompt = "Would you like to " ++ displayRequirement req
        r <- selectFromList' prompt ("Continue" :| ["Cancel Spell"])
        if fst r == 0 then testRequirement req else return False,
      requirementScale = scale req <&> (+ 2),
      monsterOnlyRequirement = False,
      displayRequirement = "You can " ++ displayRequirement req
    }

{-destroyThis :: DestroyType -> Requirement
destroyThis d =
  Requirement
    { testRequirement = case d of
        Discard -> discardSelf
        Banish -> banishSelf,
      requirementScale = case d of
        Discard -> -12
        Banish -> -14,
      monsterOnlyRequirement = False,
      displayRequirement = show d ++ " this card"
    }
  where
    cantFind = do
      name <- asks cardName
      liftIO $ putStrLn ("Cannot find " ++ name ++ ".")
      return False
    discardSelf =
      findThisCard >>= \case
        Nothing -> cantFind
        Just (_, Graveyard) -> do
          name <- asks cardName
          liftIO $ putStrLn (name ++ " is already in the Graveyard")
          return False
        Just (i, loc) -> do
          this <- ask
          lift $ do
            graveyard =: this
            toLens loc -= i
            void $ trigger OnDiscard this
            return True
    banishSelf =
      findThisCard >>= \case
        Nothing -> cantFind
        Just (i, loc) -> lift $ do
          toLens loc -= i
          return True
-}

playCardEffect :: SearchType -> Effect
playCardEffect t =
  Effect
    { performEffect = lift $ playCard t,
      monsterOnlyEffect = False,
      effectScale = return $ case t of
        ForSpell -> -3
        _ -> 0,
      displayEffect = "Play a " ++ show t
    }

-- See cards in the enemy hand?
-- Return to hand or deck from field without triggering  (req)
-- Return enemy
-- Modify search to let you pull from GY and optionally first rather than choice
-- Modify alterPower to target enemys
