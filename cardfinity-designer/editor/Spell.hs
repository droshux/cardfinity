{-# LANGUAGE OverloadedStrings #-}

module Spell (spellEditor, defaultSpell, spellName, spellTrigger, castingConditions, effects) where

import qualified Miso as M
import Miso.Lens (Lens, lens, (^.), (.=))
import Shared
import qualified Types as CF (Spell(..),Trigger(..) ,Spell )
import qualified Atoms as A
import Data.Set.Ordered as OS
import qualified Miso.Html as H
import qualified Miso.Html.Property as P
import Data.Maybe (fromMaybe)
import Miso.Binding ((<-->))
import Miso.Types ((+>))
import Conditions (conditionEditor, condition)
import Effects (effectEditor, effect)
import qualified Miso.CSS as C

data SpellAction = SetName M.MisoString | SetTrigger M.MisoString


spellEditor :: M.Component parent CF.Spell SpellAction
spellEditor = M.component  defaultSpell update view

defaultSpell :: CF.Spell
defaultSpell  = CF.Spell "" CF.OnPlay OS.empty []

update :: SpellAction -> M.Effect parent CF.Spell SpellAction
update (SetName t) = spellName .= M.fromMisoString t
update (SetTrigger t) = spellTrigger .= strTrig t

view :: CF.Spell -> M.View CF.Spell SpellAction
view m = H.div_ [] [
        H.input_ [
            P.type_ "text",
            P.value_ (M.toMisoString (m^.spellName)),
            H.onChange SetName,
            P.placeholder_ "Spell Name"
            ],
        H.select_ [
            P.value_ (trigStr $ m^.spellTrigger),
            H.onChange SetTrigger
            ] (map triggerOption [toEnum 0 ..]),
        H.div_ [C.style_ [C.borderWidth "1h",C.border "solid"]] +> conditionsEditor {M.bindings=[castingConditions <--> noLens]},
        H.div_ [C.style_ [C.borderWidth "1h",C.border "solid"]] +> effectsEditor {M.bindings=[effects <--> noLens ]}
    ]
    where
        conditionsEditor = osetEditor A.DiscardSelf condition conditionEditor 
        effectsEditor = listEditor A.DiscardEnemy effect effectEditor 

triggerOption :: CF.Trigger -> M.View model action
triggerOption t = H.option_ [P.value_ (trigStr t)] [M.text (M.toMisoString $ show t)]

spellName :: Lens CF.Spell String
spellName = lens CF._spellName $ \c n -> c {CF._spellName = n}

spellTrigger :: Lens CF.Spell CF.Trigger
spellTrigger = lens CF._spellTrigger $ \c t -> c {CF._spellTrigger = t}

castingConditions :: Lens CF.Spell (OS.OSet A.Condition)
castingConditions = lens CF._castingConditions $ \c cs -> c {CF._castingConditions = cs}

effects :: Lens CF.Spell [A.Effect]
effects = lens CF._effects $ \c es -> c {CF._effects = es}

trigStr :: CF.Trigger -> M.MisoString
trigStr CF.OnPlay  = "Play"
trigStr CF.OnDiscard = "Discard"
trigStr CF.OnDraw = "Draw"
trigStr CF.OnTap = "Tap"
trigStr CF.OnVictory = "Victory"
trigStr CF.OnDefeat = "Defeat"
trigStr CF.OnAttach = "Attach"
trigStr CF.Infinity = "Infinity"
trigStr CF.Counter = "Counter"

strTrig :: M.MisoString -> CF.Trigger
strTrig "Play" = CF.OnPlay
strTrig "Discard" = CF.OnDiscard
strTrig "Draw" = CF.OnDraw
strTrig "Tap" = CF.OnTap
strTrig "Victory" = CF.OnVictory
strTrig "Defeat" = CF.OnDefeat
strTrig "Attach" = CF.OnAttach
strTrig "Infinity" = CF.Infinity
strTrig "Counter" = CF.Counter
rtrTrig _ = CF.OnPlay
