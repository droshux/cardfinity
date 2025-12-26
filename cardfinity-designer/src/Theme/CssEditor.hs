{-# LANGUAGE OverloadedStrings #-}

module Theme.CssEditor (cssEditor) where

import Control.Monad (unless)
import Language.Javascript.JSaddle (fromJSVal, jsg, (!))
import Miso qualified as M
import Miso.Html qualified as H
import Miso.Html.Property qualified as P
import Miso.String qualified as M

parentId :: M.MisoString
parentId = "cssEditorDiv"

cssEditor :: M.Component parent M.MisoString Action
cssEditor =
  (M.component "" update view)
    { M.scripts =
        -- https://github.com/A99US/CM6-Browser-Wrapper
        [ M.Src "https://code.jquery.com/jquery-3.4.1.min.js",
          M.Src "src/Theme/cssEditor.js",
          M.Src "https://a99us.github.io/CM6-Browser-Wrapper/cm6-browser-wrapper.min.js"
        ]
    }

view :: M.MisoString -> M.View parent Action
view css =
  H.div_
    []
    [ H.button_ [H.onClick (CodeMirror NewEditor)] [M.text "Init"],
      H.button_ [H.onClick (CodeMirror SaveDoc)] [M.text "Save"],
      H.div_ [P.id_ parentId] []
    ]

data Action = CodeMirror CMAction | SetCSS M.MisoString | Noop

data CMAction = NewEditor | NewState | UpdateDoc | SaveDoc

instance M.ToMisoString CMAction where
  toMisoString NewEditor = "neweditor"
  toMisoString NewState = "newstate"
  toMisoString UpdateDoc = "updatedoc"
  toMisoString SaveDoc = "savedoc"

update :: Action -> M.Effect parent M.MisoString Action
update (CodeMirror SaveDoc) = do
  M.io_ $ cmAction SaveDoc
  let getCSS = jsg ("window" :: String) ! (parentId <> "data")
  M.io (getCSS >>= fmap (maybe Noop SetCSS) . fromJSVal)
update (CodeMirror cmd) = M.io_ $ cmAction cmd
update (SetCSS css) = M.put css
update Noop = return ()

cmAction :: CMAction -> M.JSM ()
cmAction cmd = do
  actionButton <- M.getElementById "cfCSS-command"
  M.setValue actionButton $ M.toMisoString cmd <> "-" <> parentId
  M.click () actionButton
