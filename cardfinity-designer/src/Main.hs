{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Main where

import Editor qualified
import Miso qualified as M
import Miso.Html qualified as H
import Miso.Lens (Lens, lens, (^.), _id)
import Miso.Lens.TH (makeLenses)
import ShowCard

newtype Model = Model
  { _editor :: Maybe Editor.CardModel
  }
  deriving (Eq)

$(makeLenses ''Model)

data Action

app = M.component initialState M.noop view

initialState = Model {_editor = Nothing}

view m =
  H.div_
    []
    [ H.div_ [] M.+> Editor.editor {M.bindings = [editor M.<--> Editor.currentCard]},
      case m ^. editor of
        Nothing -> H.p_ [] [M.text "No Card Selected"]
        Just card ->
          H.pre_
            []
            [ M.text $ M.toMisoString $ show $ Editor.cardFromModel 0 card
            ]
    ]

main :: IO ()
main = M.run (M.startApp app)
