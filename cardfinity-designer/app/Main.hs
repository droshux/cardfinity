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
import Types (Spell (Spell), Trigger (OnPlay))
import Spell (spellEditor)
import Data.Set.Ordered
import Data.Foldable (toList)

main :: IO ()
main = M.run (M.startApp app )

newtype Model = Model {
    _spell :: Spell
} deriving (Eq)


spell = lens _spell $ \m s -> m {_spell = s}

app = M.component (Model def) update view


def = Spell "Pot of Greed" OnPlay empty [A.Draw 2]
update _ = spell .= def

view :: Model -> M.View Model ()
view m = H.div_ [] [
    H.div_[] [
    H.button_ [H.onClick ()] [text "Reset"],
    text $ M.toMisoString $ tempShow $ m^.spell
    ],
    H.div_ [] +> (spellEditor {bindings = [spell <--> noLens ]})
    ]


tempShow :: Spell -> String
tempShow (Spell n t cs es) =  concat [
    show n,
    " ",
    show t,
    ": ",
    implode ", " (toList cs),
    " ",
    implode ", " es
    ] 

implode :: (Show a) => String -> [a] -> String
implode j [] = ""
implode j [x] = show x
implode j (x:xs) = show x ++ j ++ implode j xs
