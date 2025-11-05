{-# LANGUAGE OverloadedStrings #-}

module Monster (monsterEditor, defaultMonster) where
import qualified Miso as M
import Miso.Lens (Lens, lens, (^.), (.=), (%=))
import qualified Types as CF (Monster(..), Spell)
import qualified Data.Set.Ordered as OS
import qualified Atoms as A
import GHC.Natural (Natural)
import Text.Read (readMaybe)
import Data.Foldable (forM_)
import qualified Miso.Html as H
import qualified Miso.Html.Property as P
import Shared (listEditor, noLens, osetEditor)
import Spell (spellEditor, defaultSpell)
import Conditions (conditionEditor, condition)
import Miso.Types ((<-->), (+>))

data MonsterAction = SetName M.MisoString | SetPower M.MisoString | ToggleEntersTapped

monsterEditor :: M.Component parent CF.Monster MonsterAction
monsterEditor = M.component defaultMonster update view

defaultMonster :: CF.Monster
defaultMonster = CF.Monster "" [] OS.empty  0 False

update :: MonsterAction -> M.Effect parent CF.Monster MonsterAction
update ToggleEntersTapped = isTapped %= not
update (SetName t) = monsterName  .= M.fromMisoString  t
update (SetPower p) = do
    let mbP:: Maybe Natural = readMaybe $ M.fromMisoString p
    forM_ mbP (combatPower .=)

view :: CF.Monster -> M.View CF.Monster MonsterAction
view m = H.div_ [] [
    H.input_ [
        P.type_ "text",
        P.value_ (M.toMisoString $ m^.monsterName ),
        H.onChange SetName
    ],
    H.div_ [] +> summonConditionsEditor {M.bindings=[summoningConditions <--> noLens ]},
    H.div_ [] +> spellsEditor {M.bindings=[monsterSpells<-->noLens ]},
    H.input_ [
        P.type_ "number",
        P.value_ (M.toMisoString $ show $ m^.combatPower ),
        P.min_ "0",
        H.onChange SetPower 
    ],
    H.button_ [H.onClick ToggleEntersTapped] [M.text "Toggle"]
    ]
    where
        summonConditionsEditor = osetEditor A.DiscardSelf condition conditionEditor 
        spellsEditor = listEditor defaultSpell noLens spellEditor 

monsterName :: Lens CF.Monster String
monsterName  =  lens CF._monsterName $ \m n -> m {CF._monsterName = n}

monsterSpells :: Lens CF.Monster [CF.Spell]
monsterSpells = lens CF._monsterSpells $ \m n -> m {CF._monsterSpells = n}

summoningConditions :: Lens CF.Monster (OS.OSet A.Condition)
summoningConditions  =  lens CF._summoningConditions $ \m cs -> m {CF._summoningConditions = cs}

combatPower :: Lens CF.Monster Natural
combatPower  = lens CF._combatPower $ \m p -> m {CF._combatPower = p}

isTapped :: Lens CF.Monster Bool
isTapped  = lens CF._isTapped $ \m t -> m {CF._isTapped = t}
