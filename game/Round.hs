{-# LANGUAGE LambdaCase #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Redundant <&>" #-}

module Round (gameRound) where

import Control.Monad (when, (<=<))
import Control.Monad.Except (MonadIO (liftIO), MonadTrans (lift))
import Control.Monad.RWS (MonadReader (ask))
import Control.Monad.Reader (ReaderT (runReaderT))
import Control.Monad.Trans.Except (ExceptT)
import Control.Monad.Trans.State (StateT, modify)
import Data.Functor ((<&>))
import Data.List.NonEmpty (NonEmpty (..))
import Data.Maybe (fromMaybe)
import Optics.Operators ((.~), (^.), (^?))
import Optics.Optic ((%))
import System.Console.ANSI (clearScreen)
import Types
import Utils

gameRound :: Control.Monad.Trans.Except.ExceptT Player (StateT GameState IO) ()
gameRound = do
  let takeTurn = runReaderT $ draw >> untapAll >> action
  takeTurn Player1
  lift $ modify $ isFirstTurn .~ False
  takeTurn Player2
  gameRound

data RoundAction = Play | CheckHand | CheckField | CheckTheirField | CheckGY | CheckGYEnemy | Activate | Pass | Forfeit deriving (Enum)

instance Show RoundAction where
  show Play = "Play a Card"
  show CheckHand = "See detailed information about your hand."
  show CheckField = "See detailed information about your field."
  show CheckTheirField = "See detailed information about the opponent's field."
  show CheckGY = "See your graveyard."
  show CheckGYEnemy = "See the enemy graveyard."
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
  asOpponent $ yourLoc " " "Opponent's" Field
  liftIO $ putStr "Opponent's Hand:"
  asOpponent $ player's hand >>= liftIO . print . length
  yourLoc " " "Your" Hand
  yourLoc " " "Your" Field

  act <- selectFromList "What do you do:" allRoundActions <&> snd
  liftIO clearScreen
  case act of
    Pass -> return ()
    Forfeit -> deckout
    CheckHand -> displayLoc Hand >> action
    CheckField -> displayLoc Field >> action
    CheckTheirField -> asOpponent (displayLoc Field) >> action
    CheckGY -> showGY "Your" >> action
    CheckGYEnemy -> asOpponent (showGY "Their") >> action
    Play -> playCard ForCard >> action
    Activate -> activateCard >> action
  where
    printHP = do
      ask >>= liftIO . putStr . show
      liftIO $ putStr ": "
      player's deck >>= liftIO . print . length
    yourLoc delim s l = do
      liftIO $ do
        putStr $ s ++ " "
        putStr $ show l
        putStrLn ": "
      printCardsIn delim l
    displayLoc = mapM_ (liftIO . print) <=< player's . toLens
    showGY text = do
      yourLoc "\n" text Graveyard
      (res, _) <- selectFromList "Would you like to see more details?" $ "Yes" :| ["No"]
      when (res == 0) $ displayLoc Graveyard

activateCard :: GameOperation ()
activateCard =
  player's field <&> filter isActivatable >>= \case
    [] -> liftIO $ putStrLn "No monsters on the field can be activated."
    activatable -> do
      res <- selectFromListCancelable "Select a monster to activate:" (map cardName activatable)
      ifNotCancelled res $ \(i, _) ->
        let target = activatable !! i
         in cardElim (const $ return ()) (activateMonster target) target
  where
    manualSpell s = s ^. spellTrigger `elem` [Infinity, OnTap]
    -- isActivatable = cardElim (const False) $ \m -> not (_isTapped m) && any manualSpell (_monsterSpells m)
    isActivatable c =
      let getStats = monsterStats
          tapped = fromMaybe True $ c ^? getStats % isTapped
          manualSpells = maybe False (any manualSpell) (c ^? getStats % monsterSpells)
       in not tapped && manualSpells
    activateMonster c m = do
      let options = filter manualSpell $ m ^. monsterSpells
      res <- selectFromListCancelable "Select a monster spell to activate:" options
      ifNotCancelled res $ \(i, _) -> do
        let spell = options !! i
        let t = spell ^. spellTrigger
        -- Cast spell with c as the card context
        didCast <- flip runReaderT c $ actSpell spell t
        when (didCast && t == OnTap) $ runReaderT tapThisCard c

untapAll :: GameOperation ()
untapAll = field %= map (monsterStats % isTapped .~ False)
