{-# LANGUAGE LambdaCase #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Redundant <&>" #-}

module Atoms where

import Control.Monad.Except (MonadIO (liftIO), MonadTrans (lift), replicateM_, when)
import Control.Monad.RWS (MonadReader (ask), asks, unless)
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
toPredicate (ForFamily f) = elem f . cardFamilies
toPredicate ForCard = const True
toPredicate t = cardElim (const $ t == ForSpell) (const $ t == ForMonster)

instance Show SearchType where
  show ForCard = "card"
  show ForSpell = "spell card"
  show ForMonster = "monster card"
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

instance HasScale FindCards where
  scale (FindCards _ _ Deck) = 0
  scale (FindCards n _ Field) = -(4 * natToInt n)
  scale (FindCards n _ _) = -natToInt n

instance Requirement FindCards where
  testRequirement (FindCards n t l) = playerState' <&> h
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
      playerState' <&> deck >>= \case
        [] -> performEffect Deckout >> return False
        dck -> case findIndex (toPredicate t) dck of
          Nothing -> return False
          Just i -> do
            updatePlayerState' $ \p -> p {deck = deck p `without` i}
            handleDiscard $ dck !! i
            moveOn
    other ->
      playerState' <&> filter (toPredicate t) . toLens other >>= \case
        [] -> liftIO $ do
          putStr "Could not find any "
          putStr $ show t
          putStr $ if l == Field then " on the " else " in the "
          putStr $ show l
          return False
        (c : cs) -> do
          let names = cardName c :| map cardName cs
          (i, _) <- selectFromList' names
          upWithout other i
          handleDiscard ((c : cs) !! i)
          moveOn
    where
      moveOn = testRequirement $ Destroy d $ FindCards (n - 1) t l
      handleDiscard c = unless (d == Banish) $ lift $ do
        updatePlayerState $ \p -> p {graveyard = c : graveyard p}
        trigger OnDiscard c
      upWithout loc i = updatePlayerState' $ \p -> case loc of
        Hand -> p {hand = hand p `without` i}
        Field -> p {field = field p `without` i}
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
      playerState' <&> graveyard >>= \case
        [] -> return ()
        (_ : gy) -> updatePlayerState' $ \p -> p {graveyard = gy}
      moveOn
    Deck ->
      playerState' <&> deck >>= \case
        [] -> performEffect Deckout
        (c : deck) -> do
          updatePlayerState' $ \p -> p {deck = deck}
          handleDiscard c
          moveOn
    Hand -> do
      let setHand h p = p {hand = h}
      -- Randomly shuffle the hand
      playerState' <&> hand >>= shuffleM >>= updatePlayerState' . setHand

      -- Discard the first card in the hand
      playerState' <&> hand >>= \case
        [] -> return ()
        (c : h) -> do
          updatePlayerState' $ setHand h
          handleDiscard c
      moveOn
    Field ->
      playerState' <&> field >>= \case
        [] -> return ()
        (cfst : crst) -> do
          let names = cardName cfst :| map cardName crst
          (i, _) <- selectFromList' names
          c <- playerState' <&> (!! i) . field
          updatePlayerState' $ \p -> p {field = field p `without` i}
          handleDiscard c
          moveOn
    where
      moveOn = asOpponent' $ performEffect $ DestroyTheirs d (n - 1) l
      handleDiscard c =
        when (d == Discard) $ lift $ do
          updatePlayerState $ \p -> p {graveyard = c : graveyard p}
          trigger OnDiscard c

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
  show (Draw n) = "Draw " ++ show n ++ " card " ++ if n == 1 then "" else "s"

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
  performEffect (Peek n) = playerState' <&> deck >>= liftIO . mapM_ print . take (natToInt n)

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
    gy <- playerState' <&> graveyard
    if length gy < natToInt n
      then return False
      else do
        updatePlayerState' $ \p -> p {graveyard = drop (natToInt n) $ graveyard p}
        return True

newtype Choose = Choose (NonEmpty (Ex Effect))

instance Show Choose where
  show (Choose es) = showFold " or " es

instance HasScale Choose where
  scale (Choose es) = 3 + maximum (NonE.map scale es)

instance Effect Choose where
  performEffect (Choose es) = do
    (_, c) <- lift $ selectFromList es
    performEffect c

data Attack = Attack | AttackDirectly

instance Show Attack where
  show Attack = "Attack with this monster"
  show AttackDirectly = "Attack Directly with this monster"

instance HasScale Attack where
  scale Attack = 10
  scale AttackDirectly = 20

instance Effect Attack where
  performEffect AttackDirectly = ask >>= cardElim (const $ return ()) attackDirectly
    where
      attackDirectly m = do
        liftIO $ putStrLn "Attacking Directly!"
        didKill <- lift $ dealDamage m
        when didKill $ asOpponent' (performEffect Deckout)

      dealDamage m = do
        let discardFromDeck p = p {deck = drop (natToInt $ combatPower m) $ deck p}
        asOpponent $ updatePlayerState discardFromDeck
        asOpponent playerState <&> null . deck
  performEffect Attack = ask >>= cardElim (const $ return ()) attack
    where
      attack m =
        lift (asOpponent playerState) <&> field >>= \case
          [] -> performEffect AttackDirectly
          (efst : erst) -> do
            (i, _) <- selectFromList' $ NonE.map cardName (efst :| erst)
            let target = (efst : erst) !! i
            let targetP = cardElim (const 0) combatPower target
            if targetP > combatPower m then defeatThis else defeatTarget target

      defeatTarget = lift . asOpponent . runReaderT defeatThis
      defeatThis = do
        ask >>= lift . trigger OnDefeat

        -- Get the location of this card and send it to the graveyard
        cid <- asks cardID
        playerState' <&> findIndex ((==) cid . cardID) . field >>= \case
          Nothing -> liftIO (putStrLn "Error: Cannot find this card on the field.")
          Just i -> do
            -- Send to the graveyard
            let toGY p = p {field = (field p !! i) : graveyard p}
            let offField p = p {field = field p `without` i}
            lift $ updatePlayerState (offField . toGY)

        ask >>= lift . trigger OnDiscard
  monsterOnlyEffect = const True
