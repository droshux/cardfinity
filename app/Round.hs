{-# LANGUAGE LambdaCase #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Redundant <&>" #-}

module Round (gameRound) where

import Control.Monad (void, when, (<=<))
import Control.Monad.Except (MonadIO (liftIO), MonadTrans (lift))
import Control.Monad.RWS (MonadReader (ask))
import Control.Monad.Reader (ReaderT (runReaderT))
import Control.Monad.Trans.Except (ExceptT)
import Control.Monad.Trans.State (StateT, modify)
import Data.Functor ((<&>))
import Data.List (findIndex)
import Data.List.NonEmpty (NonEmpty (..))
import System.Console.ANSI (clearScreen)
import Types
import Utils

gameRound :: Control.Monad.Trans.Except.ExceptT Player (StateT GameState IO) ()
gameRound = do
  let takeTurn = runReaderT $ draw >> untapAll >> action
  takeTurn Player1
  lift $ modify $ \s -> s {isFirstTurn = False}
  takeTurn Player2
  gameRound

data RoundAction = Play | CheckHand | CheckField | CheckTheirField | CheckGY | Activate | Pass | Forfeit deriving (Enum)

instance Show RoundAction where
  show Play = "Play a Card"
  show CheckHand = "See detailed information about your hand."
  show CheckField = "See detailed information about your field."
  show CheckTheirField = "See detailed information about the opponent's field."
  show CheckGY = "See your graveyard."
  show Activate = "Activate an effect of a card on the field."
  show Pass = "End your turn."
  show Forfeit = "Forfeit the game."

allRoundActions :: NonEmpty RoundAction
allRoundActions = toEnum 0 :| enumFrom (toEnum 1)

action :: GameOperation ()
action = do
  -- Start of screen preamble:
  ask >>= liftIO . putStrLn . (++ "'s turn.") . show
  printHP >> asOpponent printHP
  asOpponent $ yourLoc "Opponent's" Field
  yourLoc "Your" Hand
  yourLoc "Your" Field

  act <- selectFromList "What do you do:" allRoundActions <&> snd
  liftIO clearScreen
  case act of
    Pass -> return ()
    Forfeit -> deckout
    CheckHand -> displayLoc Hand >> action
    CheckField -> displayLoc Field >> action
    CheckTheirField -> asOpponent (displayLoc Field) >> action
    CheckGY -> do
      yourLoc "Your" Graveyard
      (res, _) <- selectFromList "Would you like to see more details?" $ "Yes" :| ["No"]
      when (res == 0) $ displayLoc Graveyard
      action
    Play -> playCard >> action
    Activate -> activateCard >> action
  where
    printHP = do
      ask >>= liftIO . putStr . show
      liftIO $ putStr ": "
      playerState >>= liftIO . print . length . deck
    yourLoc s l = do
      liftIO $ do
        putStr $ s ++ " "
        putStr $ show l
        putStrLn ": "
      printCardsIn l
    displayLoc = mapM_ (liftIO . print) <=< ((playerState <&>) . toLens)

playCard :: GameOperation ()
playCard = do
  -- Select a playable card from your hand
  let playable = flip cardElim (const True) $ (== OnPlay) . spellTrigger
  playerState <&> filter playable . hand >>= \case
    [] -> liftIO $ putStrLn "No cards to play."
    canPlay -> do
      res <- selectFromListCancelable "Select a card to play:" $ map cardName canPlay
      ifNotCancelled res $ \(i, _) ->
        let toPlay = canPlay !! i
         in cardElim (playSpell toPlay) (playMonster toPlay) toPlay
  where
    -- Find c in the hand and remove it
    fromHand c =
      playerState <&> findIndex ((== cardID c) . cardID) . hand >>= \case
        Nothing -> liftIO $ putStrLn ("Error, " ++ cardName c ++ " not in Hand")
        Just i -> updatePlayerState $ \p -> p {hand = hand p `without` i}
    -- Spell: trigger OnPlay, move it to the GY
    playSpell c =
      const $
        trigger OnPlay c >>= \case
          False -> liftIO $ putStrLn ("Cannot play " ++ cardName c)
          True -> do
            updatePlayerState $ \p -> p {graveyard = c : graveyard p}
            fromHand c
    -- Monster: Test summoning conditions, move to field, trigger OnPlay
    playMonster c m = do
      success <- runReaderT (checkAll $ summoningConditions m) c
      if not success
        then liftIO $ putStrLn ("Failed to summon " ++ monsterName m)
        else do
          updatePlayerState $ \p -> p {field = c : field p}
          fromHand c
          void $ trigger OnPlay c

activateCard :: GameOperation ()
activateCard =
  playerState <&> filter isActivatable . field >>= \case
    [] -> liftIO $ putStrLn "No monsters on the field can be activated."
    activatable -> do
      res <- selectFromListCancelable "Select a monster to activate:" (map cardName activatable)
      ifNotCancelled res $ \(i, _) ->
        let target = activatable !! i
         in cardElim (const $ return ()) (activateMonster target) target
  where
    manualSpell = (`elem` [Infinity, OnTap]) . spellTrigger
    isActivatable = cardElim (const False) $ \m -> not (isTapped m) && any manualSpell (monsterSpells m)
    activateMonster c m = do
      let options = filter manualSpell $ monsterSpells m
      res <- selectFromListCancelable "Select a monster spell to activate:" options
      ifNotCancelled res $ \(i, _) -> do
        let spell = options !! i
        let t = spellTrigger spell
        -- Cast spell with c as the card context
        didCast <- flip runReaderT c $ actSpell spell t
        when (didCast && t == OnTap) $ runReaderT tapThisCard c

untapAll :: GameOperation ()
untapAll = updatePlayerState $ \p -> p {field = map untapCard $ field p}
  where
    untapCard c = cardElim (const c) (untapMonster c) c
    untapMonster c m = c {cardStats = MonsterStats $ m {isTapped = False}}
