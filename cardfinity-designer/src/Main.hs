{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Main where

import CardView qualified
import Editor qualified
import GHC.Num (integerToNatural)
import Miso qualified as M
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
    _errMsg :: M.MisoString
  }
  deriving (Eq)

$(makeLenses ''Model)

data Action = SetEditorState Editor.DeckModel | Error M.MisoString

app =
  (M.component initialState update view)
    { M.mailbox = M.checkMail SetEditorState Error
    }

initialState = Model {_deck = [], _currentCard = Nothing, _errMsg = ""}

update :: Action -> M.Effect () props Model Action
update (Error msg) = errMsg M..= msg
update (SetEditorState state) = do
  deck M..= Editor.deckFromModel state
  let i = state M.^. Editor.currentCardIndex
  let currentCardModel = state M.^. at i `M.compose` Editor.deck
  currentCard M..= fmap (Editor.cardFromModel (fromIntegral i) . snd) currentCardModel

view _ _ m =
  H.div_
    []
    [ M.text (m M.^. errMsg), -- TODO: Make error stand out
      case m M.^. currentCard of
        Nothing -> H.p_ [] [M.text "No Card Selected"]
        Just card ->
          H.div_
            []
            [ H.pre_
                []
                [ M.text $ M.toMisoString $ show card
                ],
              CardView.card False (m M.^. deck) card
            ],
      "editor" M.+> Editor.editor
    ]

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
