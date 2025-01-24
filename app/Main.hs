module Main where

import ExampleCards (sampleDeck)
import Game (runGame)

main :: IO ()
main = runGame sampleDeck sampleDeck
