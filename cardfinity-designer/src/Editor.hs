module Editor
  ( DeckModel,
    CardModel,
    currentCard,
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
import Editor.Types (CardModel, DeckAction, DeckModel, Default (def), currentCardIndex, deck)
import Editor.Update (update, (%))
import Editor.View (view)
import Miso qualified as M
import Miso.Lens (at)
import Miso.Lens qualified as M

editor :: M.Component parent DeckModel DeckAction
editor = M.component def update view

currentCard :: M.Lens DeckModel (Maybe CardModel)
currentCard =
  let ix xs i
        | i < 0 || i >= length xs = Nothing
        | otherwise = Just $ xs !! i
      get m = ix (m M.^. deck) (m M.^. currentCardIndex)
      set d Nothing = d
      set d (Just m) =
        let i = d M.^. currentCardIndex
            copies = maybe 1 fst $ d M.^. deck % at i
         in (deck % at i M..~ Just (copies, m)) d
   in M.lens (fmap snd . get) set
