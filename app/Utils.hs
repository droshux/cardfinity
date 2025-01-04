module Utils where

import Control.Monad.Except (MonadIO (liftIO), MonadPlus (mzero), MonadTrans (lift), runExceptT)
import Control.Monad.Reader (MonadReader (ask), ReaderT (runReaderT))
import Control.Monad.State (MonadState (get, put), StateT (runStateT))
import Control.Monad.Trans.Maybe (MaybeT)
import Data.Functor ((<&>))
import Text.Read (readMaybe)
import Types

sandbox :: GameOperation a -> GameOperation (Either Player a, GameState)
sandbox op = do
  currentPlayer <- ask
  initialState <- get
  liftIO $ flip runStateT initialState $ runExceptT $ runReaderT op currentPlayer

checkAll :: Card -> [Ex Requirement] -> GameOperation Bool
checkAll c rs = do
  (res, fstate) <- sandbox $ mapM (testRequirement c) rs <&> and
  case res of
    Right True -> put fstate >> return True
    _ -> return False

promoteMonsterSpell :: Card -> Spell -> Card
promoteMonsterSpell m s = m {cardStats = SpellStats s}

selectFromList :: (Show a) => [a] -> MaybeT IO (Int, a)
selectFromList xs = do
  liftIO $ printOptions xs
  liftIO $ putStrLn "(Or leave blank to cancel)"
  line <- lift getLine
  if null line
    then mzero
    else case readMaybe line of
      Just i -> return (i, xs !! i)
      Nothing -> do
        liftIO $ putStrLn "Invalid input, please try again..."
        selectFromList xs
  where
    printOptions :: (Show a) => [a] -> IO ()
    printOptions = mapM_ (\(i :: Int, s) -> putStr (show i ++ ": ") >> print s) . zip [0 ..]
