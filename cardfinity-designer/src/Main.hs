{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Main where

import CardView qualified
import Editor qualified
import GHC.Num (integerToNatural)
import Miso qualified as M
import Miso.CSS qualified as CSS
import Miso.Html qualified as H
import Miso.Lens (Lens, at, lens, _id)
import Miso.Lens qualified as M
import Miso.Lens.TH (makeLenses)
import Scale (runScale)
import ShowCard
import Types qualified as CF (Card)

data Model = Model
  { _deck :: [CF.Card],
    _currentCard :: Maybe CF.Card,
    _errMsg :: M.MisoString,
    _theme :: Theme.Theme
  }
  deriving (Eq)

$(makeLenses ''Model)

data Action = SetEditorState Editor.DeckModel | Error M.MisoString

app =
  (M.component initialState update view)
    { M.mailbox = M.checkMail SetEditorState Error
    }

initialState = Model {_deck = [], _currentCard = Nothing, _errMsg = "", _theme = Theme.defaultTheme}

update :: Action -> M.Effect () props Model Action
update (Error msg) = errMsg M..= msg
update (SetEditorState state) = do
  deck M..= Editor.deckFromModel state
  let i = state M.^. Editor.currentCardIndex
  let currentCardModel = state M.^. at i `M.compose` Editor.deck
  currentCard M..= fmap (Editor.cardFromModel (fromIntegral i) . snd) currentCardModel

view _ _ m =
  H.div_
    [ CSS.style_
        [ CSS.display "grid",
          CSS.gridTemplateColumns "auto auto",
          CSS.gridTemplateRows "min-content max-content"
        ]
    ]
    [ M.text (m M.^. errMsg), -- TODO: Make error stand out
      case m M.^. currentCard of
        Nothing -> H.div_ [] []
        Just card -> M.mountWithProps (cvProps card) CardView.cardView,
      case m M.^. currentCard of
        Nothing -> H.p_ [] [M.text "No Card Selected"]
        Just card ->
          H.pre_
            [ CSS.style_
                [ CSS.whiteSpace "pre-wrap",
                  CSS.maxWidth "100%"
                ]
            ]
            [M.text $ M.toMisoString $ show card],
      H.div_
        [ CSS.style_ [("grid-row", "1 / span2")]
        ]
        ["editor" M.+> Editor.editor],
      "themeSelector" M.+> Theme.selector {M.bindings = [theme M.<-- Theme.currentTheme]},
      M.text $ Theme.themeClass $ m ^. theme
    ]
  where
    cvProps c = CardView.CardViewProps c (m M.^. deck) (m M.^. theme)

main :: IO ()
#ifdef INTERACTIVE
main = M.liveWithContext M.defaultEvents () app
#else
main = M.startAppWithContext M.defaultEvents () app
#endif

#ifdef WASM
#ifndef INTERACTIVE
foreign export javascript "hs_start" main :: IO ()
#endif
#endif
