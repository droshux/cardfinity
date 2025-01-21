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
  testRequirement (IfCards (FindCards n t l)) = player's' (toLens l) <&> h
    where
      h = (>= natToInt n) . length . filter (toPredicate t)

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
      -- playerState' <&> filter validTarget . toLens other >>= \case
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
      moveOn = testRequirement $ Destroy d $ FindCards (n - 1) t l
      handleDiscard c = unless (d == Banish) $ do
        graveyard =: c
        void $ trigger OnDiscard c

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
      moveOn = asOpponent' $ performEffect $ DestroyTheirs d (n - 1) l
      handleDiscard c =
        when (d == Discard) $ lift $ do
          graveyard =: c
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
  performEffect (Peek n) = player's' deck >>= liftIO . mapM_ print . take (natToInt n)

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
    gy <- player's' graveyard
    if length gy < natToInt n
      then return False
      else do
        lift $ graveyard %= drop (natToInt n)
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
    ft <- gets (^. isFirstTurn)
    if not ft
      then ask >>= cardElim (const $ return ()) attackDirectly
      else liftIO $ putStrLn "You cannot attack on the first turn."
    where
      attackDirectly m = do
        liftIO $ putStrLn "Attacking Directly!"
        didKill <- lift $ asOpponent $ takeDamage m
        when didKill $ lift $ asOpponent deckout

      takeDamage m = do
        let toDiscard = natToInt $ m ^. combatPower
        dtop <- player's deck <&> take toDiscard
        graveyard %= (++ dtop)
        deck %= drop toDiscard
        player's deck <&> null
  performEffect Attack = do
    ft <- gets (^. isFirstTurn)
    if not ft
      then ask >>= cardElim (const $ return ()) attack
      else liftIO $ putStrLn "You cannot attack on the first turn."
    where
      attack m =
        lift (opponent's field) >>= \case
          -- lift (asOpponent playerState) <&> _field >>= \case
          [] -> performEffect AttackDirectly
          (efst : erst) -> do
            (i, _) <- selectFromList' "Select the monster to attack:" $ NonE.map cardName (efst :| erst)
            let target = (efst : erst) !! i
            let targetP = fromMaybe 0 $ target ^? monsterStats % combatPower
            if targetP > m ^. combatPower then defeatThis else defeatTarget target

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
    player's' deck >>= \case
      [] -> lift deckout
      (c : cs) ->
        if not $ toPredicate t c
          then do
            lift $ deck .= cs
            performEffect (DrillFor t)
          else lift $ do
            deck .= cs
            hand =: c
            void $ trigger OnDraw c
  performEffect (SearchFor t) =
    options >>= \case
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
    where
      options = player's' deck <&> filter (toPredicate t)

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
    player's' graveyard >>= \case
      [] -> liftIO $ putStrLn "No more cards in the Graveyard."
      (c : cs) -> do
        lift $ do
          deck =: c
          graveyard .= cs
        performEffect $ Heal (n - 1)
  performEffect DrawGY =
    lift $
      player's graveyard >>= \case
        [] -> liftIO $ putStrLn "No more cards in the Graveyard."
        (c : cs) -> do
          hand =: c
          graveyard .= cs
          void $ trigger OnDraw c
  performEffect PlayGY =
    lift $
      player's graveyard >>= \case
        [] -> liftIO $ putStrLn "No more cards in the Graveyard."
        (c : _) ->
          if c ^? (spellStats % spellTrigger) /= Just OnTap
            then liftIO $ putStrLn (cardName c ++ " is not playable.")
            else
              let spellCase = ifMeets (spellStats % castingConditions) triggerPlay
                  monsterCase = ifMeets (monsterStats % summoningConditions) monstGY
               in cardElim' (const spellCase) (const monsterCase) c
    where
      ifMeets = (. maybe (return ()) . (checkAll >=>) . flip when) . (>>=) . asks . flip (^?)
      triggerPlay = ask >>= lift . void . trigger OnPlay
      monstGY = do
        -- Move to Field
        player's' graveyard >>= \case
          [] -> liftIO $ putStrLn "No cards in the graveyard."
          (c : cs) -> lift $ do
            field =: c
            graveyard .= cs
        triggerPlay

data Attach = Attach

instance Show Attach where
  show = const "Attach a spell card from your hand to this card."

instance HasScale Attach where
  scale = const 10

instance Effect Attach where
  performEffect =
    const $
      player's' hand <&> mapMaybe (^? spellStats) >>= \case
        [] -> liftIO $ putStrLn "There are no spell cards in your hand."
        (sfst : srst) -> do
          (i, s) <- selectFromList' "Select a spell to attach:" (sfst :| srst)
          findThisCard >>= \case
            Nothing -> liftIO $ putStrLn ("Error, " ++ s ^. spellName ++ " cannot be found!")
            Just p -> lift $ do
              liftIO $ putStr ("Attaching " ++ s ^. spellName ++ " to ")
              hand -= i
              updateCard p (attach s)
    where
      updateCard (i, loc) f = do
        before <- player's (toLens loc)
        let c = before !! i

        liftIO $ putStrLn $ cardName c

        let setTo = take i before ++ [f c] ++ drop (i + 1) before
        toLens loc .= setTo
      attach s = monsterStats % monsterSpells %~ (s :)
  monsterOnlyEffect = const True
