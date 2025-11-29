{-# LANGUAGE CPP #-}
{-# LANGUAGE TemplateHaskell #-}

module Main where

import Editor qualified
import Miso qualified as M
import Miso.Html qualified as H
import Miso.Lens (Lens, lens, _id)
import Miso.Lens.TH (makeLenses)

newtype Model = Model
  { _editor :: Editor.DeckModel
  }
  deriving (Eq)

$(makeLenses ''Model)

data Action

app = M.component initialState M.noop view

initialState = Model {_editor = Editor.initialState}

view _ =
  H.div_
    []
    [ H.div_ [] M.+> Editor.editor {M.bindings = [editor M.<--> _id]}
    ]

main :: IO ()
main = M.run (M.startApp app)
