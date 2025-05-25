{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-missing-signatures #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Redundant <&>" #-}

module Main (main) where

import CardParser (card, deck)
import Control.Monad (forM_, void)
import Data.Foldable (toList)
import Data.Functor ((<&>))
import Data.List (nub)
import Data.Maybe (mapMaybe)
import Data.Text (pack)
import Data.Version (showVersion)
import Game (runGame)
import Graphics.PDF (PDFRect (PDFRect), mkStdFont, runPdf)
import Graphics.PDF.Document (PDFDocumentInfo (..), standardDocInfo)
import Graphics.PDF.Fonts.StandardFont (FontName (Times_Roman))
import Optics.Operators ((^.))
import Options.Applicative.Simple (addCommand, help, metavar, simpleOptions, strArgument)
import ParserCore (space)
import Paths_cardfinity (version)
import Pdf (document, pageDimension)
import System.Exit (exitFailure)
import Text.Megaparsec (errorBundlePretty, manyTill, parse)
import Text.Megaparsec.Byte (string')
import Types (Card, cardFamilies, cardName, isLegal, runScale)
import Utils (SearchType (ForFamily, ForName), rarity)

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
        pdf
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
  -- Read entire deck
  dck <- fst3 <$> tryParseDeck path

  -- Read all unique cards and print
  let cardsParse = manyTill (card <* space) (string' "deck:")
  cs <-
    readFile path <&> parse cardsParse path >>= \case
      Left err -> do
        putStrLn $ errorBundlePretty err
        exitFailure
      Right cs -> return cs

  -- Print all cards and their scale or any legality errors!
  forM_ cs $ \c -> do
    print c
    case runScale dck c of
      Left err -> print err
      Right s -> do
        putStr "Scale: " >> print s
        putStr "Rarity: " >> putStr (rarity dck $ ForName $ cardName c)
        putStrLn ""

  putStrLn ""
  void $ isLegal dck -- Exits if this fails!
  putStrLn "Rarity of all families:"
  forM_ (allFamilies dck) $ \(f, r) -> do
    putStr $ show f ++ ": "
    putStrLn r
  putStr "Number of cards: "
  print $ length dck
  putStr "Total scale: "
  print (sum $ mapMaybe ((\case Left _ -> Nothing; Right x -> Just x) . runScale dck) dck)
  return ()
  where
    allFamilies dck = map (\x -> (x, rarity dck $ ForFamily x)) $ nub $ foldr (\x a -> a ++ toList (x ^. cardFamilies)) [] dck

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

tryParseDeck :: String -> IO ([Card], String, String)
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

fst3 (x, _, _) = x

playWithDecks :: (String, String) -> IO ()
playWithDecks p = bothM (fmap fst3 . tryParseDeck) p >>= uncurry runGame

pdf =
  addCommand
    "pdf"
    "Generate a PDF of a deck for printing."
    genPDF
    $ strArgument
    $ metavar "Deck" <> help "The file containing the deck to generate a PDF of."

genPDF :: String -> IO ()
genPDF path = do
  (cs, dName, dAuthor) <- tryParseDeck path
  {- fnt <- mkStdFont Times_Roman >>= \case
      Left err -> do
          print err
          exitFailure
      Right f -> return f -}
  let info = standardDocInfo {author = pack dAuthor, compressed = False}
  runPdf (dName ++ ".pdf") info pageDimension $ document cs
