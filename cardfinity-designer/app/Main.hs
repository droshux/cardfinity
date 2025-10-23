{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

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
    _exEffect :: A.FindCards
} deriving (Eq)

exEffect = lens _exEffect $ \r e -> r {_exEffect  = e}

app = M.component (Model $ A.FindCardsHand 1 A.ForCard) noop view

view :: Model -> M.View Model ()
view m = H.div_ [] [
    H.p_ [] [ (text . toMisoString . show) (m ^. exEffect)],
    H.div_ [] +> (findCardsEditor {bindings = [exEffect <-- findCards ]})
    ]

{- data ChildModel = CM {
    _valid :: Bool,
    _info :: M.MisoString
} deriving Eq

$(makeLenses  ''ChildModel)

data ChildAction = Toggle | SetText M.MisoString

child :: Component parent ChildModel ChildAction 
child = M.component (CM False "") update view where 
        update Toggle = valid %= not
        update (SetText t) = info .= t
        view m = H.span_ [] [
                H.button_ [H.onClick Toggle] [text "Toggle"],
                H.input_ [H.onInput SetText]
            ]

childLens :: Lens ChildModel (Maybe M.MisoString)
childLens = let 
    get (CM v i) = if v then Just i else Nothing
    set (CM _ i) Nothing = CM False i
    set (CM v _) (Just i) = CM v i
    in lens get set

newtype ParentModel = PM {
    _mbInfo :: Maybe M.MisoString
} deriving Eq

$(makeLenses  ''ParentModel)

parent :: Component a ParentModel ()
parent = M.component (PM Nothing) (const $ return ()) view
    where
        misoShow Nothing = "Nothing"
        misoShow (Just i) = i
        view m = H.div_ [] [
            H.p_ [] [text "Data: ", text $ misoShow $ m ^. mbInfo],
            H.div_ [] +> (child  {bindings=[mbInfo <-- childLens ]})
            ]
        

main :: IO ()
main = M.run (M.startApp parent ) -}
