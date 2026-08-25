{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Main where

import CardView qualified
import Context (Context, initialCtx, theme)
import Editor qualified
import Miso qualified as M
import Miso.CSS qualified as CSS
import Miso.Html qualified as H
import Miso.Lens (at)
import Miso.Lens qualified as M
import Miso.Lens.TH (makeLenses)
import Theme qualified
import Types qualified as CF (Card, DeckInfo (..))

data Model = Model
  { _deck :: CF.DeckInfo,
    _currentCard :: Maybe CF.Card,
    _errMsg :: M.MisoString
  }
  deriving (Eq)

$(makeLenses ''Model)

data Action = SetEditorState Editor.DeckModel | Error M.MisoString

app :: M.Component Context () Model Action
app =
  (M.component initialState update view)
    { M.mailbox = M.checkMail SetEditorState Error
    }

initialState :: Model
initialState =
  Model
    { _deck =
        CF.DeckInfo
          { CF.deckName = "",
            CF.author = "",
            CF.deckList = []
          },
      _currentCard = Nothing,
      _errMsg = ""
    }

update :: Action -> M.Effect Context props Model Action
update (Error msg) = errMsg M..= msg
update (SetEditorState state) = do
  deck M..= Editor.deckFromModel state
  let i = state M.^. Editor.currentCardIndex
  let currentCardModel = state M.^. at i `M.compose` Editor.deck
  currentCard M..= fmap (Editor.cardFromModel (fromIntegral i) . snd) currentCardModel

view :: Context -> props -> Model -> M.View Context Model Action
view ctx _ m =
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
      H.div_
        [ CSS.style_ []
        ]
        ["editor" M.+> Editor.editor],
      H.div_
        []
        [ "themeSelector" M.+> Theme.selector,
          M.text $ Theme.themeClass $ ctx M.^. theme
        ]
    ]
  where
    cvProps = flip CardView.CardViewProps (m M.^. deck)

main :: IO ()
#ifdef INTERACTIVE
main = M.liveWithContext M.defaultEvents initialCtx app
#else
main = M.startAppWithContext M.defaultEvents initialCtx app
#endif

#ifdef WASM
#ifndef INTERACTIVE
foreign export javascript "hs_start" main :: IO ()
#endif
#endif
