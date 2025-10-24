{-# LANGUAGE CPP #-}

module Main where

import Miso (text, toMisoString, Component (bindings))
import Miso.Lens.TH (makeLenses)
import Miso qualified as M
import Miso.Html qualified as H
import Atoms (FindCards)
import qualified Atoms as A
import Miso.Effect (noop)
import Miso.Lens (Lens, lens, (^.), (.=), (%=))
import Shared (findCardsEditor, findCards)
import Miso.Types ( (<--), (+>) )
import Effects (effectEditor, effect)

main :: IO ()
main = M.run (M.startApp app )

newtype Model = Model {
    _exEffect :: A.Effect
} deriving (Eq)

exEffect = lens _exEffect $ \r e -> r {_exEffect  = e}

app = M.component (Model A.DiscardEnemy) noop view

view :: Model -> M.View Model ()
view m = H.div_ [] [
    H.p_ [] [ (text . toMisoString . show) (m ^. exEffect)],
    H.div_ [] +> (effectEditor {bindings = [exEffect <-- effect ]})
    ]

