module Editor
  ( DeckModel,
    CardModel,
    currentCardIndex,
    deckFromModel,
    cardFromModel,
    editor,
    deck,
    (%),
    def,
  )
where

import Editor.Mapping (cardFromModel, deckFromModel)
import Editor.Types (CardModel, DeckAction (UpdateParent), DeckModel, Default (def), currentCardIndex, deck)
import Editor.Update (update, (%))
import Editor.View (view)
import Miso qualified as M

editor :: M.Component ctx props DeckModel DeckAction
editor =
  (M.component def update view)
    { M.mount = Just UpdateParent
    }
