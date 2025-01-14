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
import Types
import Utils (asOpponent, deckout, draw, playerState, printCardsIn, selectFromList)

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
      res <- selectFromList ("Yes" :| ["No"]) <&> fst
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
playCard = _

activateCard :: GameOperation ()
activateCard = _
