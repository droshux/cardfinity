{-# OPTIONS_GHC -Wno-orphans #-}

module Utils where

import Control.Monad.Except (MonadIO (liftIO), MonadPlus (mzero), MonadTrans (lift), forM_, runExceptT, when)
import Control.Monad.Reader (MonadReader (ask, local), ReaderT (runReaderT), asks)
import Control.Monad.State (MonadState (get, put), StateT (runStateT), gets, modify)
import Control.Monad.Trans.Maybe (MaybeT (runMaybeT))
import Data.Foldable (Foldable (toList))
import Data.Functor ((<&>))
import Data.Set.Ordered (OSet)
import GHC.Natural (Natural, naturalToInteger)
import GHC.Num (integerToInt)
import Text.Read (readMaybe)
import Types

natToInt :: Natural -> Int
natToInt = integerToInt . naturalToInteger

showFold :: (Show a, Foldable f) => String -> f a -> String
showFold connector = foldr (\x a -> a ++ connector ++ show x) ""

sandbox :: GameOperation a -> GameOperation (Either Player a, GameState)
sandbox op = do
  currentPlayer <- ask
  initialState <- get
  liftIO $ flip runStateT initialState $ runExceptT $ runReaderT op currentPlayer

checkAll :: OSet (Ex Requirement) -> GameOperation Bool
checkAll rs = do
  (res, fstate) <- sandbox $ mapM testRequirement (toList rs) <&> and
  case res of
    Right True -> put fstate >> return True
    _ -> return False

promoteMonsterSpell :: Card -> Spell -> Card
promoteMonsterSpell m s = m {cardStats = SpellStats s}

selectFromList :: (Show a) => [a] -> GameOperation (Maybe (Int, a))
selectFromList = liftIO . runMaybeT . helper
  where
    printOptions = mapM_ printOption . zip [0 ..]
    helper xs = do
      liftIO $ printOptions xs
      liftIO $ putStrLn "(Or leave blank to cancel)"
      line <- lift getLine
      if null line
        then mzero
        else case readMaybe line of
          Just i -> return (i, xs !! i)
          Nothing -> do
            liftIO $ putStrLn "Invalid input, please try again..."
            helper xs
    printOption (i :: Int, s) = putStrLn (show i ++ ": " ++ show s)

playerState :: GameOperation PlayerState
playerState = do
  stateGetter <- asks (getPlayerState . currentPlayer)
  gets stateGetter

updatePlayerState :: (PlayerState -> PlayerState) -> GameOperation ()
updatePlayerState f = asks (h . currentPlayer) >>= modify
  where
    h Player1 gs = gs {player1State = f $ player1State gs}
    h Player2 gs = gs {player2State = f $ player2State gs}

trigger :: Trigger -> Card -> GameOperation ()
trigger t c = local (\gc -> gc {cardContext = c}) activateCard
  where
    activateCard = asks cardContext >>= cardElim activateSpell activateMonster
    activateSpell s = when (spellTrigger s == t) $ do
      r <- checkAll (castingConditions s)
      if not r
        then liftIO $ putStrLn ("Can't cast " ++ spellName s)
        else forM_ (effects s) performEffect
    activateMonster m
      | isMonsterOnly t = do
          r <- selectFromList $ getOptions m
          case r of
            Nothing -> liftIO $ putStrLn "Cancelled!"
            Just (_, s) -> activateSpell s
      | otherwise = mapM_ activateSpell $ getOptions m
    getOptions = filter ((==) t . spellTrigger) . monsterSpells

instance Show Spell where
  show (Spell n t cs es) = concat [show n, " ", show t, " ", scs, ": ", ses]
    where
      scs = showFold ", " cs
      ses = showFold ", " es

showCardCount :: Natural -> String
showCardCount 0 = "no cards"
showCardCount 1 = "a card"
showCardCount n = show n ++ " cards"
