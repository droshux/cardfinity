{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Main where

import Editor qualified
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
    [ "card editor" M.+> Editor.editor {M.bindings = [editor M.<--> _id]},
      case m ^. editor Editor.% Editor.currentCard of
        Nothing -> H.p_ [] [M.text "No Card Selected"]
        Just card ->
          let z = flip runScale (Editor.cardFromModel 0 card) $ Editor.deckFromModel $ m ^. editor
           in H.div_
                []
                [ M.text $ M.toMisoString (either show show z),
                  H.pre_
                    []
                    [ M.text $ M.toMisoString $ show $ Editor.cardFromModel 0 card
                    ]
                ]
    ]

main :: IO ()
main = M.run (M.startApp app)
