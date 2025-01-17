module Main where

import Game (runGame)
import ExampleCards (sampleDeck)

main :: IO ()
main = runGame sampleDeck sampleDeck
