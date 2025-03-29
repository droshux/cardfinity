module Main where

import CardParser (card)
import Control.Applicative (empty)
import Options.Applicative.Simple (metavar, short, simpleOptions, strOption)
import Text.Megaparsec (parseTest)

main :: IO ()
main = do
  let opt = strOption (short 'f' <> metavar "Card File")
  (path :: String, ()) <- simpleOptions "ver" "header" "desc" opt empty
  readFile path >>= parseTest card
