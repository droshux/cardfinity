{-# OPTIONS_GHC -Wno-name-shadowing #-}

module Game (runGame) where

import Control.Monad.Except (runExceptT)
import Control.Monad.Random (MonadRandom)
import Control.Monad.State (StateT (runStateT))
import Optics.Operators ((.~))
import Round (gameRound)
import Scale (isLegal)
import System.Random.Shuffle (shuffleM)
import Types

initGameState :: (MonadRandom m) => [Card] -> [Card] -> m GameState
initGameState d1 d2 = do
  d1'' <- shuffleM d1
  d2'' <- shuffleM d2
  let (d1', d2') = prepareDecks d1'' d2''
  return
    GameState
      { _player1State =
          PlayerState
            { _hand = take 4 d1',
              _graveyard = [],
              _field = [],
              _deck = drop 4 d1',
              _autotapList = []
            },
        _player2State =
          PlayerState
            { _hand = take 5 d2',
              _graveyard = [],
              _field = [],
              _deck = drop 5 d2',
              _autotapList = []
            },
        _isFirstTurn = True
      }
  where
    prepareDecks :: [Card] -> [Card] -> ([Card], [Card])
    prepareDecks cs1 cs2 =
      let setID n = cardID .~ fromIntegral n
          setIDsToRange a b = zipWith setID [a .. a + b]
       in (setIDsToRange 0 (length cs1) cs1, setIDsToRange (length cs1) (length cs2) cs2)

runGame :: [Card] -> [Card] -> IO ()
runGame d1 d2 =
  do
    d1 <- isLegal d1
    d2 <- isLegal d2
    gs <- initGameState d1 d2
    (result, _) <- flip runStateT gs $ runExceptT gameRound
    case result of
      Right () -> putStrLn "Somehow the game has ended without a winner! Most likely a glitch!"
      Left loser -> putStrLn $ case loser of
        Player1 -> "Player 2 wins!"
        Player2 -> "Player 1 wins!"
