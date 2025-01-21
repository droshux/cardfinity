{-# LANGUAGE LambdaCase #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Redundant <&>" #-}

module Atoms
  ( SearchType (..),
    toPredicate,
    FindCards (..),
    IfCards (..),
    DestroyType (..),
    DestroyCards (..),
    DestroyTheirs (..),
    Deckout (..),
    Draw (..),
    Peek (..),
    PopGraveyard (..),
    Choose (..),
    Attack (..),
    SearchForCard (..),
    Healing (..),
    Attach (..),
  )
where

import Control.Monad (void, (>=>))
import Control.Monad.Except (MonadIO (liftIO), MonadTrans (lift), replicateM_, when)
import Control.Monad.RWS (MonadReader (ask), asks, gets, unless)
import Control.Monad.Reader (ReaderT (runReaderT))
import Data.Functor ((<&>))
import Data.List (findIndex, stripPrefix)
import Data.List.NonEmpty (NonEmpty (..))
import Data.List.NonEmpty qualified as NonE
import GHC.Natural (Natural)
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
toPredicate (ForFamily f) = elem f . _cardFamilies
toPredicate ForCard = const True
toPredicate t = cardElim (const $ t == ForSpell) (const $ t == ForMonster)

instance Show SearchType where
  show ForCard = "card"
  show ForSpell = "spell"
  show ForMonster = "monster"
  show (ForName n) = show n
  show (ForFamily f) = show f ++ " card"

data FindCards = FindCards Natural SearchType CardLocation deriving (Ord, Eq)

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

newtype IfCards = IfCards FindCards deriving (Ord, Eq)

instance HasScale IfCards where
  scale (IfCards (FindCards _ _ Deck)) = 0
  scale (IfCards (FindCards n _ Field)) = -(4 * natToInt n)
  scale (IfCards (FindCards n _ _)) = -natToInt n

instance Show IfCards where
  show (IfCards f) = case f of
    FindCards n _ _ -> "if there" ++ isare n ++ "at least " ++ show f
    where
      isare n = if n == 1 then " is " else " are "

instance Requirement IfCards where
  testRequirement (IfCards (FindCards n t l)) = playerState' <&> h
    where
      h = (>= natToInt n) . length . filter (toPredicate t) . toLens l

data DestroyType = Discard | Banish deriving (Eq, Ord, Show)

-- Destroy your own cards as a requirement
data DestroyCards = Destroy DestroyType FindCards deriving (Eq, Ord)

instance HasScale DestroyCards where
  scale (Destroy _ (FindCards _ ForSpell Field)) = 0
  scale (Destroy Discard (FindCards _ _ Graveyard)) = 0
  scale (Destroy Banish f) = scale (Destroy Discard f) - case f of (FindCards n _ _) -> 2 * natToInt n
  scale (Destroy Discard (FindCards n _ loc)) =
    natToInt n * case loc of
      Deck -> -5
      Hand -> -10
      Field -> -15

instance Show DestroyCards where
  show (Destroy d f) = show d ++ " " ++ show f

instance Requirement DestroyCards where
  -- No effect => Always succeeds, Base Case => Always succeeds
  testRequirement (Destroy _ (FindCards 0 _ _)) = return True
  -- Cannot discard from graveyard
  testRequirement (Destroy Discard (FindCards _ _ Graveyard)) = return False
  testRequirement (Destroy d (FindCards n t l)) = case l of
    Deck ->
      playerState' <&> _deck >>= \case
        [] -> lift deckout
        dck -> case findIndex (toPredicate t) dck of
          Nothing -> return False
          Just i -> do
            let c = dck !! i
            liftIO $ putStrLn (show d ++ "ing " ++ cardName c ++ " from the Deck")
            updatePlayerState' $ \p -> p {_deck = _deck p `without` i}
            lift $ handleDiscard c
            moveOn
    other -> do
      cid <- asks _cardID
      let validTarget c = toPredicate t c && _cardID c /= cid
      playerState' <&> filter validTarget . toLens other >>= \case
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
          playerState' <&> findIndex ((== _cardID c) . _cardID) . toLens other >>= \case
            Nothing -> liftIO $ putStrLn ("Error, " ++ cardName c ++ " not in " ++ show other)
            Just j -> lift $ do
              upWithout other j
              handleDiscard c
          moveOn
    where
      moveOn = testRequirement $ Destroy d $ FindCards (n - 1) t l
      handleDiscard c = unless (d == Banish) $ do
        updatePlayerState $ \p -> p {_graveyard = c : _graveyard p}
        void $ trigger OnDiscard c
      upWithout loc i = updatePlayerState $ \p -> case loc of
        Hand -> p {_hand = _hand p `without` i}
        Field -> p {_field = _field p `without` i}
        _ -> p

-- Destroy their cards as an effect
data DestroyTheirs = DestroyTheirs DestroyType Natural CardLocation

instance Show DestroyTheirs where
  show (DestroyTheirs d n l) = replaceLast " the " " the enemy " destroyOurs
    where
      destroyOurs = show $ Destroy d $ FindCards n ForCard l
      replaceLast b a s = reverse $ replaceFirst (reverse b) (reverse a) (reverse s)
      replaceFirst _ _ [] = []
      replaceFirst b a (c : cs) = case stripPrefix b (c : cs) of
        Just cs' -> a ++ cs'
        Nothing -> c : replaceFirst b a cs

instance HasScale DestroyTheirs where
  -- This might be changed to use values other than 5 10 15
  scale (DestroyTheirs d n l) = -scale (Destroy d (FindCards n ForCard l))

instance Effect DestroyTheirs where
  performEffect (DestroyTheirs _ 0 _) = return () -- Base case
  performEffect (DestroyTheirs d n l) = asOpponent' $ case l of
    Graveyard -> when (d == Banish) $ do
      playerState' <&> _graveyard >>= \case
        [] -> return ()
        (_ : gy) -> updatePlayerState' $ \p -> p {_graveyard = gy}
      moveOn
    Deck ->
      playerState' <&> _deck >>= \case
        [] -> lift deckout
        (c : deck) -> do
          updatePlayerState' $ \p -> p {_deck = deck}
          handleDiscard c
          moveOn
    Hand -> do
      let setHand h p = p {_hand = h}
      -- Randomly shuffle the hand
      playerState' <&> _hand >>= shuffleM >>= updatePlayerState' . setHand

      -- Discard the first card in the hand
      playerState' <&> _hand >>= \case
        [] -> return ()
        (c : h) -> do
          updatePlayerState' $ setHand h
          handleDiscard c
      moveOn
    Field ->
      playerState' <&> _field >>= \case
        [] -> return ()
        (cfst : crst) -> do
          let names = cardName cfst :| map cardName crst
          (i, _) <- selectFromList' ("Select one of the opponent's monsters to " ++ show d ++ ":") names
          c <- playerState' <&> (!! i) . _field
          updatePlayerState' $ \p -> p {_field = _field p `without` i}
          handleDiscard c
          moveOn
    where
      moveOn = asOpponent' $ performEffect $ DestroyTheirs d (n - 1) l
      handleDiscard c =
        when (d == Discard) $ lift $ do
          updatePlayerState $ \p -> p {_graveyard = c : _graveyard p}
          void $ trigger OnDiscard c

data Deckout = Deckout deriving (Eq, Ord)

instance Show Deckout where
  show = const "DECKOUT"

instance HasScale Deckout where
  scale = const (-50)

instance Effect Deckout where
  -- Cancells everything early and ends the game
  performEffect _ = lift deckout

newtype Draw = Draw Natural

instance Show Draw where
  show (Draw n) = "Draw " ++ show n ++ " card" ++ if n == 1 then " " else "s"

instance HasScale Draw where
  scale (Draw n) = natToInt n * 10

instance Effect Draw where
  performEffect (Draw n) = replicateM_ (natToInt n) (lift draw)

newtype Peek = Peek Natural

instance Show Peek where
  show (Peek n) =
    concat
      [ "See the top ",
        if n == 1 then "" else show n ++ " ",
        "card",
        if n == 1 then "" else "s",
        " of the deck"
      ]

instance HasScale Peek where
  scale (Peek n) = 2 ^ n

instance Effect Peek where
  performEffect (Peek n) = playerState' <&> _deck >>= liftIO . mapM_ print . take (natToInt n)

newtype PopGraveyard = PopGraveyard Natural deriving (Eq, Ord)

instance Show PopGraveyard where
  show (PopGraveyard n) =
    concat
      [ "Banish the top ",
        if n == 1 then "" else show n ++ " ",
        "card",
        if n == 1 then "" else "s",
        " of the graveyard"
      ]

instance HasScale PopGraveyard where
  scale (PopGraveyard n) = -(2 * natToInt n)

instance Requirement PopGraveyard where
  testRequirement (PopGraveyard n) = do
    gy <- playerState' <&> _graveyard
    if length gy < natToInt n
      then return False
      else do
        updatePlayerState' $ \p -> p {_graveyard = drop (natToInt n) $ _graveyard p}
        return True

newtype Choose = Choose (NonEmpty (Ex Effect))

instance Show Choose where
  show (Choose es) = showFold " or " es

instance HasScale Choose where
  scale (Choose es) = maximum (NonE.map scale es)

instance Effect Choose where
  performEffect (Choose es) = do
    (_, c) <- lift $ selectFromList "Choose one of the following:" es
    performEffect c

data Attack = Attack | AttackDirectly

instance Show Attack where
  show Attack = "Attack with this monster"
  show AttackDirectly = "Attack Directly with this monster"

instance HasScale Attack where
  scale Attack = 10
  scale AttackDirectly = 20

instance Effect Attack where
  performEffect AttackDirectly = do
    ft <- gets _isFirstTurn
    if not ft
      then ask >>= cardElim (const $ return ()) attackDirectly
      else liftIO $ putStrLn "You cannot attack on the first turn."
    where
      attackDirectly m = do
        liftIO $ putStrLn "Attacking Directly!"
        didKill <- lift $ asOpponent $ takeDamage m
        when didKill $ lift $ asOpponent deckout

      takeDamage m = do
        let toDiscard = natToInt $ _combatPower m
        updatePlayerState $ \p -> p {_graveyard = take toDiscard (_deck p) ++ _graveyard p}
        updatePlayerState $ \p -> p {_deck = drop toDiscard $ _deck p}
        playerState <&> null . _deck
  performEffect Attack = do
    ft <- gets _isFirstTurn
    if not ft
      then ask >>= cardElim (const $ return ()) attack
      else liftIO $ putStrLn "You cannot attack on the first turn."
    where
      attack m =
        lift (asOpponent playerState) <&> _field >>= \case
          [] -> performEffect AttackDirectly
          (efst : erst) -> do
            (i, _) <- selectFromList' "Select the monster to attack:" $ NonE.map cardName (efst :| erst)
            let target = (efst : erst) !! i
            let targetP = cardElim (const 0) _combatPower target
            if targetP > _combatPower m then defeatThis else defeatTarget target

      defeatTarget = lift . asOpponent . runReaderT defeatThis
      defeatThis = do
        ask >>= lift . void . trigger OnDefeat

        -- Get the location of this card and send it to the graveyard
        cid <- asks _cardID
        mbIndex <- playerState' <&> findIndex ((==) cid . _cardID) . _field
        case mbIndex of
          Nothing -> liftIO (putStrLn "Error: Cannot find this card on the field.")
          Just i -> do
            -- Send to the graveyard
            updatePlayerState' $ \p -> p {_graveyard = (_field p !! i) : _graveyard p}
            -- Remove from field
            updatePlayerState' $ \p -> p {_field = _field p `without` i}

        ask >>= lift . void . trigger OnDiscard
  monsterOnlyEffect = const True

data SearchForCard = SearchFor SearchType | DrillFor SearchType

instance Show SearchForCard where
  show (SearchFor t) = "Search the deck for a " ++ show t
  show (DrillFor t) = "Drill the deck for a " ++ show t

instance HasScale SearchForCard where
  scale (SearchFor _) = 25
  scale (DrillFor _) = 20

instance Effect SearchForCard where
  performEffect (DrillFor t) =
    playerState' <&> _deck >>= \case
      [] -> lift deckout
      (c : cs) ->
        if not $ toPredicate t c
          then do
            setDeck cs
            performEffect (DrillFor t)
          else do
            setDeck cs
            updatePlayerState' $ \p -> p {_hand = c : _hand p}
            lift $ void $ trigger OnDraw c
    where
      setDeck cs = updatePlayerState' $ \p -> p {_deck = cs}
  performEffect (SearchFor t) =
    options >>= \case
      [] -> liftIO $ putStrLn ("No " ++ show t ++ "s in the deck.")
      (cfst : crst) -> do
        ids <- options <&> map _cardID
        (i', _) <- selectFromList' "Select card to draw:" $ NonE.map cardName $ cfst :| crst
        playerState' <&> findIndex ((ids !! i' ==) . _cardID) . _deck >>= \case
          Nothing -> liftIO $ putStrLn $ cardName ((cfst : crst) !! i') ++ " not found in deck?!"
          Just i -> do
            c <- playerState' <&> (!! i) . _deck
            updatePlayerState' $ \p -> p {_hand = c : _hand p, _deck = _deck p `without` i}
            lift shuffleDeck
            lift $ void $ trigger OnDraw c
    where
      options = playerState' <&> filter (toPredicate t) . _deck

data Healing = Heal Natural | DrawGY | PlayGY

instance HasScale Healing where
  scale (Heal n) = 7 * natToInt n
  scale DrawGY = 30
  scale PlayGY = 30

instance Show Healing where
  show DrawGY = "Draw the top card of the Graveyard"
  show PlayGY = "Play the top card of the Graveyard"
  show (Heal 1) = "Put the top card of the Graveyard onto the Deck"
  show (Heal n) = "Put the top " ++ show n ++ " cards of the Graveyard onto the Deck"

instance Effect Healing where
  performEffect (Heal 0) = return ()
  performEffect (Heal n) =
    playerState' <&> _graveyard >>= \case
      [] -> liftIO $ putStrLn "No more cards in the Graveyard."
      (c : cs) -> do
        updatePlayerState' $ \p -> p {_deck = c : _deck p, _graveyard = cs}
        performEffect $ Heal (n - 1)
  performEffect DrawGY =
    lift $
      playerState <&> _graveyard >>= \case
        [] -> liftIO $ putStrLn "No more cards in the Graveyard."
        (c : cs) -> do
          updatePlayerState (\p -> p {_hand = c : _hand p, _graveyard = cs})
          void $ trigger OnDraw c
  performEffect PlayGY =
    lift $
      playerState <&> _graveyard >>= \case
        [] -> liftIO $ putStrLn "No more cards in the Graveyard."
        (c : _) ->
          if not $ cardElim ((OnPlay ==) . _spellTrigger) (const True) c
            then liftIO $ putStrLn (cardName c ++ " is not playable.")
            else
              let playSpell = test _castingConditions triggerPlay
                  playMonster = test _summoningConditions monstGY
               in cardElim' playSpell playMonster c
    where
      test = flip (.) (flip when) . (>=>) . (checkAll .)
      triggerPlay = ask >>= lift . void . trigger OnPlay
      monstGY = do
        -- Move to Field
        playerState' <&> _graveyard >>= \case
          [] -> return ()
          (c : cs) -> updatePlayerState' $ \p -> p {_field = c : _field p, _graveyard = cs}

        triggerPlay

data Attach = Attach

instance Show Attach where
  show = const "Attach a spell card from your hand to this card."

instance HasScale Attach where
  scale = const 10

instance Effect Attach where
  performEffect =
    const $
      playerState' <&> filter (cardElim (const True) (const False)) . _hand >>= \case
        [] -> liftIO $ putStrLn "There are no spell cards in your hand."
        (sfst : srst) -> do
          (i, s) <- selectFromList' "Select a spell to attach:" (sfst :| srst)
          updatePlayerState' $ \p -> p {_hand = _hand p `without` i}
          findThisCard >>= \case
            Nothing -> liftIO $ putStrLn ("Error, " ++ cardName s ++ " cannot be found!")
            Just p -> do
              liftIO $ putStr ("Attaching " ++ cardName s ++ " to ")
              lift $ updateCard p $ \c -> cardElim (const c) (flip attach c $ _cardStats s) c
    where
      updateCard :: (Int, CardLocation) -> (Card -> Card) -> GameOperation ()
      updateCard (i, loc) f = do
        before <- playerState <&> toLens loc
        let c = before !! i
        liftIO $ putStrLn $ cardName c
        let setTo = take i before ++ [f c] ++ drop (i + 1) before
        updatePlayerState $ setLoc loc setTo
      setLoc Hand cs p = p {_hand = cs}
      setLoc Deck cs p = p {_deck = cs}
      setLoc Field cs p = p {_field = cs}
      setLoc Graveyard cs p = p {_graveyard = cs}

      attach :: CardStats -> Card -> Monster -> Card
      attach (MonsterStats _) c _ = c
      attach (SpellStats s) c m = c {_cardStats = MonsterStats $ m {_monsterSpells = s : _monsterSpells m}}
  monsterOnlyEffect = const True
