{-# LANGUAGE LambdaCase #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Redundant <&>" #-}

module Round where

import Control.Monad (when, (<=<))
import Control.Monad.Except (MonadIO (liftIO))
import Control.Monad.RWS (MonadReader (ask))
import Control.Monad.Reader (ReaderT (runReaderT))
import Control.Monad.Trans.Except (ExceptT)
import Control.Monad.Trans.State (StateT)
import Data.Functor ((<&>))
import Data.List.NonEmpty (NonEmpty (..))
import Data.List.NonEmpty qualified as NonE
import Types
import Utils

gameRound :: Control.Monad.Trans.Except.ExceptT Player (StateT GameState IO) ()
gameRound = do
  let takeTurn = runReaderT $ draw >> action
  takeTurn Player1
  takeTurn Player2

data RoundAction = Play | CheckHand | CheckField | CheckGY | Activate | Pass | Forfeit

instance Show RoundAction where
  show Play = "Play a Card"
  show CheckHand = "See detailed information about your hand."
  show CheckField = "See detailed information about your field."
  show CheckGY = "See your graveyard."
  show Activate = "Activate an effect of a card on the field."
  show Pass = "End your turn."
  show Forfeit = "Forfeit the game."

allRoundActions :: NonEmpty RoundAction
allRoundActions = Play :| [CheckHand, CheckField, CheckGY, Activate, Pass, Forfeit]

action :: GameOperation ()
action = do
  -- Start of screen preamble:
  ask >>= liftIO . putStrLn . (++ "'s turn.") . show
  printHP >> asOpponent printHP
  yourLoc Hand
  yourLoc Field

  selectFromList allRoundActions <&> snd >>= \case
    Pass -> return ()
    Forfeit -> deckout
    CheckHand -> displayLoc Hand >> action
    CheckField -> displayLoc Field >> action
    CheckGY -> do
      yourLoc Graveyard
      liftIO $ putStrLn "Would you like to see more details?"
      (res, _) <- selectFromList ("Yes" :| ["No"])
      when (res == 1) $ displayLoc Graveyard
      action
    Play -> playCard >> action
    Activate -> activateCard >> action
  where
    printHP = do
      ask >>= liftIO . putStr . show
      liftIO $ putStr ": "
      playerState >>= liftIO . print . length . deck
    yourLoc l = do
      liftIO $ do
        putStr "Your "
        print l
        putStrLn ": "
      printCardsIn l
    displayLoc = mapM_ (liftIO . print) <=< ((playerState <&>) . toLens)

playCard :: GameOperation ()
playCard = do
  -- Select a playable card from your hand
  let playable = flip cardElim (const True) $ (== OnPlay) . spellTrigger
  playerState <&> filter playable . hand >>= \case
    [] -> liftIO $ putStrLn "No cards to play."
    (cfst : crst) -> do
      (i, _) <- selectFromList $ NonE.map cardName (cfst :| crst)
      let toPlay = (cfst : crst) !! i
      cardElim (playSpell i toPlay) (playMonster i toPlay) toPlay
  where
    fromHand i p = p {hand = hand p `without` i}
    -- Spell: trigger OnPlay, move it to the GY
    playSpell i c _ = do
      trigger OnPlay c
      let toGY p = p {graveyard = c : graveyard p}
      updatePlayerState (toGY . fromHand i)
    -- Monster: Test summoning conditions, move to field, trigger OnPlay
    playMonster i c m = do
      success <- runReaderT (checkAll $ summoningConditions m) c
      if not success
        then liftIO $ putStrLn ("Failed to summon " ++ monsterName m)
        else do
          let toField p = p {field = c : field p}
          updatePlayerState (toField . fromHand i)
          trigger OnPlay c

activateCard :: GameOperation ()
activateCard =
  playerState <&> filter isActivatable . field >>= \case
    [] -> liftIO $ putStrLn "No monsters on the field can be activated."
    (cfst : crst) -> do
      (i, _) <- selectFromList $ NonE.map cardName $ cfst :| crst
      -- playerState >>= cardElim (const $ return ()) activateMonster . (!! i) . field
      target <- playerState <&> (!! i) . field
      cardElim (const $ return ()) (activateMonster target) target
  where
    manualSpell = (`elem` [Infinity, OnTap]) . spellTrigger
    isActivatable = cardElim (const False) $ \m -> any manualSpell $ monsterSpells m
    activateMonster c m = case filter manualSpell $ monsterSpells m of
      -- Impossible case
      [] -> liftIO $ putStrLn "This monster has no spells that can be activated."
      (sfst : srst) -> do
        (i, _) <- selectFromList $ NonE.map spellName $ sfst :| srst
        flip trigger c $ spellTrigger $ (sfst : srst) !! i
