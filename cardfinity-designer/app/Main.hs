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
import Types (Spell (Spell), Trigger (OnPlay), Monster (..))
import Spell (spellEditor)
import Data.Set.Ordered
import Data.Foldable (toList)
import Monster (monsterEditor)

main :: IO ()
main = M.run (M.startApp app )

newtype Model = Model {
    _monster :: Monster
} deriving (Eq)


monster = lens _monster $ \m m' -> m {_monster = m'}

app = M.component (Model def) update view


def = Monster {_summoningConditions=empty , _monsterSpells=[], _monsterName="", _isTapped=False, _combatPower=0}
update _ = monster .= def

view :: Model -> M.View Model ()
view m = H.div_ [] [
    H.div_[] [
    H.button_ [H.onClick ()] [text "Reset"],
    text $ M.toMisoString $ tempShowMonster $ m^.monster
    ],
    H.div_ [] +> (monsterEditor {bindings = [monster <--> noLens ]})
    ]


tempShowMonster :: Monster -> String
tempShowMonster (Monster n ss cs p t) = concat [
    show n,
    ":\n",
    implode ", " (toList cs),
    "\n",
    implode "\n" (map tempShowSpell ss),
    "\n",
    show p,
    " ",
    show t
    ] 

tempShowSpell :: Spell -> String
tempShowSpell (Spell n t cs es) =  concat [
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
