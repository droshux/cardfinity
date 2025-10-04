{-# LANGUAGE CPP #-}

module Main where

import Miso (text, toMisoString, Component (bindings))
import Miso qualified as M
import Miso.Html qualified as H
import Atoms (FindCards)
import qualified Atoms as A
import Miso.Effect (noop)
import Miso.Lens (lens, (^.))
import Shared (findCardsEditor, findCards)
import Miso.Types ( (<--), (+>) )

main :: IO ()
main = M.run (M.startApp app)

newtype Model = Model {
    _findCards :: FindCards
} deriving (Eq)

findCardsTop = lens _findCards $ \r f -> r {_findCards = f}

app = M.component (Model $ A.FindCardsHand 0 A.ForCard) noop view

view :: Model -> M.View Model ()
view m = H.div_ [] [
    H.p_ [] [ (text . toMisoString . show) (m ^. findCardsTop)],
    H.div_ [] +> (findCardsEditor {bindings = [findCardsTop <-- findCards]})
    ]
