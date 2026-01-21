{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Main where

import CardView qualified
import Editor qualified
import GHC.Num (integerToNatural)
import Miso qualified as M
import Miso.Html qualified as H
import Miso.Lens (Lens, lens, (^.), _id)
import Miso.Lens.TH (makeLenses)
import Scale (runScale)
import ShowCard

newtype Model = Model
  { _editor :: Editor.DeckModel
  }
  deriving (Eq)

$(makeLenses ''Model)

data Action

app = M.component initialState M.noop view

initialState = Model {_editor = Editor.def}

view m =
  H.div_
    []
    [ case m ^. editor Editor.% Editor.currentCard of
        Nothing -> H.p_ [] [M.text "No Card Selected"]
        Just card ->
          H.div_
            []
            [ H.pre_
                []
                [ M.text $ M.toMisoString $ show $ currentCard m card
                ],
              CardView.card False (Editor.deckFromModel $ m ^. editor) $ currentCard m card
            ],
      "editor" M.+> Editor.editor {M.bindings = [editor M.<--> _id]}
    ]

currentCard m c = flip Editor.cardFromModel c $ fromIntegral $ m ^. editor Editor.% Editor.currentCardIndex

#ifdef WASM
#ifndef INTERACTIVE
foreign export javascript "hs_start" main :: IO ()
#endif
#endif

main :: IO ()
#ifdef INTERACTIVE
main = M.reload (M.startComponent M.defaultEvents app)
#else
main = M.run (M.startComponent M.defaultEvents app)
#endif
