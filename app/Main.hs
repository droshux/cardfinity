module Main where

import ExampleCards (sampleDeck, sampleDeck2)
import Game (runGame)

main :: IO ()
main = runGame sampleDeck sampleDeck2
