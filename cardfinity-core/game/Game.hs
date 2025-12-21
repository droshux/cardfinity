{-# OPTIONS_GHC -Wno-name-shadowing #-}

module Game (runGame) where

import Control.Monad (void)
import Control.Monad.Except (runExceptT)
import Control.Monad.Random (MonadRandom)
import Control.Monad.State (StateT (runStateT))
import Optics.Operators ((.~), (^.))
import Optics.Optic ((%))
import Round (gameRound)
import Scale (isLegal)
import System.Console.ANSI (clearScreen)
import System.Random.Shuffle (shuffleM)
import Types

initGameState :: (MonadRandom m) => [Card] -> [Card] -> m GameState
initGameState d1 d2 = do
  d1'' <- shuffleM d1
  d2'' <- shuffleM d2
  let (d1', d2') = prepareDecks d1'' d2''
  return $ initialGameState (splitAt 4 d1') (splitAt 5 d2')
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
    showP2StartingHand (gs ^. player2State % hand)
    (result, _) <- flip runStateT gs $ runExceptT gameRound
    case result of
      Right () -> putStrLn "Somehow the game has ended without a winner! Most likely a glitch!"
      Left loser -> putStrLn $ case loser of
        Player1 -> "Player 2 wins!"
        Player2 -> "Player 1 wins!"

showP2StartingHand :: [Card] -> IO ()
showP2StartingHand hand = do
  putStrLn "Player 2's starting hand:\t(Hit enter to view)"
  void getLine
  mapM_ print hand
  putStrLn "Hit enter to begin game."
  void getLine
  clearScreen
