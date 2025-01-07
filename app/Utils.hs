{-# OPTIONS_GHC -Wno-orphans #-}

module Utils where

import Control.Monad.Except (MonadIO (liftIO), MonadPlus (mzero), MonadTrans (lift), runExceptT, when)
import Control.Monad.Reader (MonadReader (ask), ReaderT (runReaderT), asks)
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

sandbox :: GameOpWithCardContext a -> GameOpWithCardContext (Either Player a, GameState)
sandbox op = do
  context <- ask
  currentPlayer <- lift ask
  initialState <- get
  let runReaders = flip runReaderT currentPlayer . flip runReaderT context
  let runMisc = flip runStateT initialState . runExceptT
  liftIO $ runMisc $ runReaders op

checkAll :: OSet (Ex Requirement) -> GameOpWithCardContext Bool
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
  stateGetter <- asks getPlayerState
  gets stateGetter

updatePlayerState :: (PlayerState -> PlayerState) -> GameOperation ()
updatePlayerState f = asks h >>= modify
  where
    h Player1 gs = gs {player1State = f $ player1State gs}
    h Player2 gs = gs {player2State = f $ player2State gs}

trigger :: Trigger -> Card -> GameOperation ()
trigger t = runReaderT activateCard
  where
    activateCard = ask >>= cardElim actSpell actMonster
    actSpell s = when (spellTrigger s == t) $ do
      r <- checkAll (castingConditions s)
      if not r
        then
          liftIO $ putStrLn ("Can't cast " ++ spellName s)
        else mapM_ performEffect $ effects s
    actMonster m
      | isMonsterOnly t = do
          r <- lift $ selectFromList $ validMSpells m
          case r of
            Nothing -> liftIO $ putStrLn "No monster spells activated."
            Just (_, s) -> actSpell s
      | otherwise = mapM_ actSpell $ validMSpells m
    validMSpells = filter ((t ==) . spellTrigger) . monsterSpells

instance Show Spell where
  show (Spell n t cs es) = concat [show n, " ", show t, " ", scs, ": ", ses]
    where
      scs = showFold ", " cs
      ses = showFold ", " es
