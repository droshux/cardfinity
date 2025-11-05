{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Miso (text, toMisoString, Component (bindings))
import Miso.Lens.TH (makeLenses)
import Miso qualified as M
import Miso.Html qualified as H
import Atoms (FindCards)
import qualified Atoms as A
import Miso.Effect (noop)
import Miso.Lens (Lens, lens, (^.), (.=), (%=))
import Shared (findCardsEditor, findCards, noLens, listEditor)
import Miso.Types ( (<-->), (+>) )
import qualified Data.List.NonEmpty as NE (NonEmpty ((:|)))
import Effects (effectEditor, effect)

main :: IO ()
main = M.run (M.startApp app )

newtype Model = Model {
    _exEffects :: [ A.Effect ]
} deriving (Eq)

exEffects = lens _exEffects $ \r e -> r {_exEffects  = e}

app = M.component (Model []) update view

update _ = exEffects .= []

view :: Model -> M.View Model ()
view m = H.div_ [] [
    H.p_ [] [ (text . toMisoString . show) (m ^. exEffects)],
    H.button_ [H.onClick ()] [text "Reset"],
    H.div_ [] +> (effectListEditor {bindings = [exEffects <--> noLens ]})
    ]
    where effectListEditor = listEditor A.DiscardEnemy effect effectEditor 


