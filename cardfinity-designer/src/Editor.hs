module Editor
  ( DeckModel,
    initialState,
    editor,
  )
where

import Editor.Types (DeckAction, DeckModel, Default (def))
import Editor.Update (update)
import Editor.View (view)
import Miso qualified as M

initialState :: DeckModel
initialState = def

editor :: M.Component parent DeckModel DeckAction
editor = M.component def update view
