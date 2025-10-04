{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Miso (text)
import Miso qualified as M
import Miso.Html qualified as H

main :: IO ()
main = M.run (M.startApp app)







app :: M.App M.MisoString M.MisoString
app = M.component "" M.put viewModel

viewModel :: M.MisoString -> M.View M.MisoString M.MisoString
viewModel curr = H.div_ [] [
    H.select_ [H.onChange id ] [
        H.option_ [] [text "opt1"],
        H.option_ [] [text "opt2"]
        ],
    H.p_ [] [text curr]
    ]
