{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Miso qualified as M

main :: IO ()
main = M.run (M.startApp app)

app = M.component () M.noop $ const $ M.text "WIP"
