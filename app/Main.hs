module Main where

import ExampleCards (chaosFlame, sampleDeck)
import Game (runGame)

main :: IO ()
main = runGame (replicate 60 chaosFlame) sampleDeck
