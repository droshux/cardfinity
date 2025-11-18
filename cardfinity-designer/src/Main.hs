{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Atoms (FindCards)
import Atoms qualified as A
import Data.Foldable (toList)
import Data.List.NonEmpty qualified as NE (NonEmpty ((:|)))
import Data.Set.Ordered
import Editor.Monster (monsterEditor)
import Editor.Shared (noLens)
import Editor.Spell (spellEditor)
import Miso (Component (bindings), text, toMisoString)
import Miso qualified as M
import Miso.CSS qualified as CSS
import Miso.Effect (noop)
import Miso.Html qualified as H
import Miso.Lens (Lens, lens, (%=), (.=), (^.))
import Miso.Lens.TH (makeLenses)
import Miso.Types ((+>), (<-->))
import Scale (runScale)
import ShowCard ()
import Types (Monster (..), Spell (Spell), Trigger (OnPlay))

main :: IO ()
main = M.run (M.startApp app)

newtype Model = Model
  { _monster :: Monster
  }
  deriving (Eq)

monster = lens _monster $ \m m' -> m {_monster = m'}

app = M.component (Model def) update view

def = Monster {_summoningConditions = empty, _monsterSpells = [], _monsterName = "", _isTapped = False, _combatPower = 0}

update _ = monster .= def

view :: Model -> M.View Model ()
view m =
  H.div_
    []
    [ H.div_
        []
        [ text (M.toMisoString $ show $ runScale [] (m ^. monster)),
          H.button_ [H.onClick ()] [text "Reset"],
          H.pre_
            [ CSS.style_
                [ CSS.border "solid",
                  CSS.width "fit-content"
                ]
            ]
            [text $ M.toMisoString $ show $ m ^. monster]
        ],
      H.div_ [] +> (monsterEditor {bindings = [monster <--> noLens]})
    ]
