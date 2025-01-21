module Game (runGame) where

import Control.Monad.Except (runExceptT)
import Control.Monad.Random (MonadRandom)
import Control.Monad.State (StateT (runStateT))
import Round (gameRound)
import System.Random.Shuffle (shuffleM)
import Types

initGameState :: (MonadRandom m) => [Card] -> [Card] -> m GameState
initGameState d1 d2 = do
  d1'' <- shuffleM d1
  d2'' <- shuffleM d2
  let (d1', d2') = prepareDecks d1'' d2''
  return
    GameState
      { _player1State = PlayerState {_hand = take 4 d1', _graveyard = [], _field = [], _deck = drop 4 d1'},
        _player2State = PlayerState {_hand = take 5 d2', _graveyard = [], _field = [], _deck = drop 5 d2'},
        _isFirstTurn = True
      }
  where
    prepareDecks :: [Card] -> [Card] -> ([Card], [Card])
    prepareDecks cs1 cs2 =
      let h a b = zipWith (\n c -> c {_cardID = fromIntegral n}) [a .. a + b]
       in (h 0 (length cs1) cs1, h (length cs1) (length cs2) cs2)

runGame :: [Card] -> [Card] -> IO ()
runGame d1 d2 =
  if not (isLegalDeck $ map _cardStats d1) || not (isLegalDeck $ map _cardStats d2)
    then putStrLn "At least one deck is illegal sorry."
    else do
      gs <- initGameState d1 d2
      (result, _) <- flip runStateT gs $ runExceptT gameRound
      case result of
        Right () -> putStrLn "Somehow the game has ended without a winner! Most likely a glitch!"
        Left loser -> putStrLn $ case loser of
          Player1 -> "Player 2 wins!"
          Player2 -> "Player 1 wins!"
