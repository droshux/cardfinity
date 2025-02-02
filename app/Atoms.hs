{-# LANGUAGE LambdaCase #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Redundant <&>" #-}

module Atoms
  ( SearchType (..),
    toPredicate,
    FindCards (..),
    ifCards,
    DestroyType (..),
    destroyCards,
    destroyTheirCards,
    deckoutEffect,
    drawEffect,
    peek,
    popGraveyard,
    choose,
    attack,
    heal,
    attach,
    youMay,
    search,
    SearchMethod,
    asEffect,
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
import Data.Maybe (fromMaybe, mapMaybe)
import GHC.Natural (Natural)
import Optics.Operators ((%~), (^.), (^?))
import Optics.Optic ((%))
import System.Random.Shuffle (shuffleM)
import Types
import Utils

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

instance Show SearchType where
  show ForCard = "card"
  show ForSpell = "spell"
  show ForMonster = "monster"
  show (ForName n) = show n
  show (ForFamily f) = show f ++ " card"

data FindCards = FindCards Natural SearchType CardLocation deriving (Eq)

instance Show FindCards where
  show (FindCards n t l) =
    concat
      [ show n,
        " ",
        show t,
        if n == 1 then " " else "s ",
        case l of Field -> "o"; _ -> "i",
        "n the ",
        show l
      ]

ifCards :: FindCards -> Requirement
ifCards (FindCards n t l) =
  def
    { displayRequirement = "If there" ++ isare n ++ "at least " ++ show (FindCards n t l),
      requirementScale = case l of
        Deck -> 0
        Field -> 4 * (-natToInt n)
        _ -> -natToInt n,
      testRequirement = player's' (toLens l) <&> h
    }
  where
    isare i = if i == 1 then " is " else " are "
    h = (>= natToInt n) . length . filter (toPredicate t)

data DestroyType = Discard | Banish deriving (Eq, Ord, Show)

chooseToDestroy :: DestroyType -> FindCards -> GameOpWithCardContext Bool
-- No effect => Always succeeds, Base Case => Always succeeds
chooseToDestroy _ (FindCards 0 _ _) = return True
-- Cannot discard from graveyard
chooseToDestroy Discard (FindCards _ _ Graveyard) = return False
chooseToDestroy d (FindCards n t l) = case l of
  Deck ->
    player's' deck >>= \case
      [] -> lift deckout
      dck -> case findIndex (toPredicate t) dck of
        Nothing -> return False
        Just i -> do
          let c = dck !! i
          liftIO $ putStrLn (show d ++ "ing " ++ cardName c ++ " from the Deck")
          lift $ do
            deck -= i
            handleDiscard c
          moveOn
  other -> do
    cid <- asks (^. cardID)
    let validTarget c = toPredicate t c && c ^. cardID /= cid
    player's' (toLens other) <&> filter validTarget >>= \case
      [] -> liftIO $ do
        putStr "Could not find a "
        putStr $ show t
        putStr $ if l == Field then " on the " else " in the "
        print l
        return False
      (cfst : crst) -> do
        let names = cardName cfst :| map cardName crst
        (i, _) <- selectFromList' ("Select a card to " ++ show d ++ ":") names
        let c = (cfst : crst) !! i
        liftIO $ putStrLn (show d ++ "ing " ++ cardName c ++ " from the " ++ show other)
        -- playerState' <&> findIndex ((== _cardID c) . _cardID) . toLens other >>= \case
        player's' (toLens other) <&> findIndex (\c' -> c' ^. cardID == c ^. cardID) >>= \case
          Nothing -> liftIO $ putStrLn ("Error, " ++ cardName c ++ " not in " ++ show other)
          Just j -> lift $ do
            toLens other -= j
            handleDiscard c
        moveOn
  where
    moveOn = chooseToDestroy d $ FindCards (n - 1) t l
    handleDiscard c = unless (d == Banish) $ do
      graveyard =: c
      void $ trigger OnDiscard c

destroyForced :: DestroyType -> Natural -> CardLocation -> GameOpWithCardContext ()
destroyForced _ 0 _ = return () -- Base case
destroyForced d n l = case l of
  Graveyard -> when (d == Banish) $ do
    player's' graveyard >>= \case
      [] -> return ()
      (_ : gy) -> lift $ graveyard .= gy
    moveOn
  Deck ->
    player's' deck >>= \case
      [] -> lift deckout
      (c : dck) -> do
        lift $ deck .= dck
        handleDiscard c
        moveOn
  Hand -> do
    -- Randomly shuffle the hand
    lift $ player's hand >>= shuffleM >>= (hand .=)

    -- Discard the first card in the hand
    player's' hand >>= \case
      [] -> return ()
      (c : h) -> do
        lift $ hand .= h
        handleDiscard c
    moveOn
  Field ->
    player's' field >>= \case
      [] -> return ()
      (cfst : crst) -> do
        let names = cardName cfst :| map cardName crst
        (i, _) <- selectFromList' ("Select one of the opponent's monsters to " ++ show d ++ ":") names
        c <- player's' field <&> (!! i)
        lift $ field -= i
        handleDiscard c
        moveOn
  where
    moveOn = destroyForced d (n - 1) l
    handleDiscard c =
      when (d == Discard) $ lift $ do
        graveyard =: c
        void $ trigger OnDiscard c

-- Destroy your own cards as a requirement
destroyCards :: DestroyType -> FindCards -> Requirement
destroyCards d (FindCards n t l) =
  def
    { requirementScale = case (d, t, l) of
        (_, ForSpell, Field) -> 0
        (Discard, _, Graveyard) -> 0
        (Banish, _, Graveyard) -> -(2 * natToInt n)
        (Banish, _, _) -> requirementScale (destroyCards Discard fc) - (2 * natToInt n)
        _ ->
          natToInt n * case l of
            Deck -> -5
            Hand -> -10
            Field -> -15,
      displayRequirement = show d ++ " " ++ show fc,
      testRequirement = chooseToDestroy d fc
    }
  where
    fc = FindCards n t l

-- Destroy their cards as an effect
destroyTheirCards :: DestroyType -> Natural -> CardLocation -> Effect
destroyTheirCards d n l =
  def
    { effectScale = -scale inverted,
      displayEffect =
        let destroyOurs = show inverted
         in replaceLast " the " " the enemy " destroyOurs,
      performEffect = asOpponent' $ destroyForced d n l
    }
  where
    inverted = destroyCards d $ FindCards n ForCard l
    replaceLast b a s = reverse $ replaceFirst (reverse b) (reverse a) (reverse s)
    replaceFirst _ _ [] = []
    replaceFirst b a (c : cs) = case stripPrefix b (c : cs) of
      Just cs' -> a ++ cs'
      Nothing -> c : replaceFirst b a cs

deckoutEffect :: Effect
deckoutEffect =
  def
    { displayEffect = "DECKOUT",
      effectScale = -50,
      performEffect = lift deckout
    }

drawEffect :: Natural -> Effect
drawEffect n =
  def
    { effectScale = natToInt n * 10,
      displayEffect = "Draw " ++ show n ++ " card" ++ if n == 1 then " " else "s",
      performEffect = replicateM_ (natToInt n) (lift draw)
    }

peek :: Natural -> Effect
peek n =
  def
    { effectScale = 2 ^ n,
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

popGraveyard :: Natural -> Requirement
popGraveyard n =
  def
    { requirementScale = -(2 * natToInt n),
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
      effectScale = maximum (NonE.map scale es),
      performEffect = do
        (_, c) <- lift $ selectFromList "Choose one of the following:" es
        performEffect c
    }

attack :: Bool -> Effect
attack directly =
  Effect
    { displayEffect = "Attack" ++ if directly then " directly " else " " ++ "with this monster",
      effectScale = if directly then 20 else 10,
      monsterOnlyEffect = True,
      performEffect = do
        let attackType = if directly then attackDirectly else attackIndirect
        ft <- gets (^. isFirstTurn)
        if not ft
          then ask >>= cardElim (const $ return ()) attackType
          else liftIO $ putStrLn "You cannot attack on the first turn."
    }
  where
    attackDirectly m = do
      liftIO $ putStrLn "Attacking Directly!"
      didKill <- lift $ asOpponent $ takeDamage m
      when didKill $ lift $ asOpponent deckout
    attackIndirect m =
      lift (opponent's field) >>= \case
        -- lift (asOpponent playerState) <&> _field >>= \case
        [] -> performEffect $ attack True
        (efst : erst) -> do
          (i, _) <- selectFromList' "Select the monster to attack:" $ NonE.map cardName (efst :| erst)
          let target = (efst : erst) !! i
          let targetP = fromMaybe 0 $ target ^? monsterStats % combatPower
          if targetP > m ^. combatPower then defeatThis else defeatTarget target
    takeDamage m = do
      let toDiscard = natToInt $ m ^. combatPower
      dtop <- player's deck <&> take toDiscard
      graveyard %= (++ dtop)
      deck %= drop toDiscard
      player's deck <&> null
    defeatTarget = lift . asOpponent . runReaderT defeatThis
    defeatThis = do
      ask >>= lift . void . trigger OnDefeat

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

      ask >>= lift . void . trigger OnDiscard

data SearchMethod = SearchFor SearchType | DrillFor SearchType

search :: SearchMethod -> Effect
search (SearchFor t) =
  def
    { effectScale = 30,
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
    { effectScale = 15,
      displayEffect = "Drill the deck for a " ++ show t,
      performEffect =
        player's' deck >>= \case
          [] -> lift deckout
          (c : cs) ->
            if not $ toPredicate t c
              then do
                lift $ deck .= cs
                performEffect $ search (DrillFor t)
              else lift $ do
                deck .= cs
                hand =: c
                void $ trigger OnDraw c
    }

heal :: Natural -> Effect
heal n =
  Effect
    { performEffect = healHelper n,
      monsterOnlyEffect = False,
      effectScale = 7 * natToInt n,
      displayEffect = "Heal " ++ show n ++ " HP"
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

attach :: Effect
attach =
  Effect
    { performEffect =
        player's' hand <&> mapMaybe (^? spellStats) >>= \case
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
      effectScale = 10,
      displayEffect = "Attach a spell card from your hand to this card"
    }
  where
    updateCard (i, loc) f = do
      before <- player's (toLens loc)
      let c = before !! i

      liftIO $ putStrLn $ cardName c

      let setTo = take i before ++ [f c] ++ drop (i + 1) before
      toLens loc .= setTo

youMay :: Effect -> Effect
youMay e =
  Effect
    { performEffect = do
        let prompt = "Would you like to " ++ displayEffect e
        r <- selectFromList' prompt ("Yes" :| ["No"])
        when (fst r == 0) $ performEffect e,
      monsterOnlyEffect = False,
      effectScale = max (-10) $ scale e,
      displayEffect = "You may " ++ show e
    }

asEffect :: Requirement -> Effect
asEffect r =
  Effect
    { performEffect = void $ testRequirement r,
      monsterOnlyEffect = False,
      effectScale = max (-10) $ scale r,
      displayEffect = displayRequirement r
    }
