{-# LANGUAGE LambdaCase #-}
{-# OPTIONS_GHC -Wno-missing-signatures #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Redundant <&>" #-}

module Main where

import CardParser (deck)
import Control.Monad (forM_)
import Data.Functor ((<&>))
import Data.Version (showVersion)
import Game (runGame)
import Optics.Operators ((^.))
import Options.Applicative.Simple (addCommand, help, metavar, simpleOptions, strArgument)
import Paths_cardfinity (version)
import System.Exit (exitFailure)
import Text.Megaparsec (errorBundlePretty, parse)
import Types (Card, HasScale (scale), cardStats, isLegal, isLegalDeck)

main :: IO ()
main = do
  ((), action) <-
    simpleOptions
      (showVersion version)
      "header"
      "desc"
      (pure ())
      $ do
        dev
        play
  action

dev =
  addCommand
    "dev"
    "Show a deck with extra information."
    devMode
    $ strArgument
      ( metavar "Deck File"
          <> help "The file to read the deck from."
      )

devMode path = do
  cs <- tryParseDeck path
  forM_ cs $ \c -> do
    print c
    putStr "Legal: "
    print $ isLegal (c ^. cardStats)
  putStr "Total deck scale: "
  print $ sum $ map (max 0 . scale) cs
  putStr "Card count: "
  print $ length cs
  putStr "Deck Legality: "
  print $ isLegalDeck $ map (^. cardStats) cs

play =
  addCommand
    "play"
    "Play 'cardfinity'"
    playWithDecks
    ((,) <$> dck '1' <*> dck '2')
  where
    dck n =
      let repl 'X' = n
          repl o = o
       in strArgument $ metavar (map repl "Player X's Deck") <> help (map repl "The file containing Player X's deck.")

tryParseDeck :: String -> IO [Card]
tryParseDeck path =
  readFile path <&> parse deck path >>= \case
    Left err -> do
      putStrLn $ errorBundlePretty err
      exitFailure
    Right cs -> return cs

bothM :: (Monad m) => (a -> m b) -> (a, a) -> m (b, b)
bothM f (x, y) = do
  x' <- f x
  y' <- f y
  return (x', y')

playWithDecks :: (String, String) -> IO ()
playWithDecks p = bothM tryParseDeck p >>= uncurry runGame
