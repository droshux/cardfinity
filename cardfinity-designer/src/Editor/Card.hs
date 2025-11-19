{-# LANGUAGE OverloadedStrings #-}

module Editor.Card (cardEditor, defaultCard) where

import Data.Set.Ordered qualified as OS
import Editor.Monster (defaultMonster, monsterEditor)
import Editor.Shared (atomEditor, bind, noLens, osetEditor)
import Editor.Spell (defaultSpell, spellEditor)
import Miso qualified as M
import Miso.CSS qualified as CSS
import Miso.Html qualified as H
import Miso.Html.Property qualified as P
import Miso.Lens qualified as M
import Miso.Types ((+>), (<-->))
import Types qualified as C

cardEditor :: M.Component parent C.Card CardEditorAction
cardEditor = M.component defaultCard update view

type CardEditorAction = Maybe M.MisoString

defaultCard :: C.Card
defaultCard =
  C.Card
    { C._cardStats = C.SpellStats defaultSpell,
      C._cardImageUrl = Nothing,
      C._cardID = 0,
      C._cardFamilies = OS.empty
    }

update :: CardEditorAction -> M.Effect parent C.Card CardEditorAction
update mbs = M.modify $ \c -> c {C._cardImageUrl = fmap M.fromMisoString mbs}

view :: C.Card -> M.View C.Card CardEditorAction
view c =
  H.div_
    []
    [ H.div_ [] +> cardStatsEditor {M.bindings = [cardStats <--> cardStatsEditorLens]},
      H.div_ [] +> familiesEditor {M.bindings = [families <--> noLens]}
    ]
  where
    familiesEditor = osetEditor "" noLens $ M.component "" M.put $ \s -> H.input_ [P.value_ $ M.toMisoString s, H.onChange M.fromMisoString]
    families = M.lens C._cardFamilies $ \c fs -> c {C._cardFamilies = fs}
    cardStats = M.lens C._cardStats $ \c cs -> c {C._cardStats = cs}

cardStatsEditorLens = M.lens snd (const set)
  where
    set cs@(C.SpellStats _) = (0, cs)
    set cs@(C.MonsterStats _) = (1, cs)

cardStatsEditor =
  let setS _ (C.SpellStats s) = s
      setS s _ = s
      setM _ (C.MonsterStats m) = m
      setM m _ = m
   in atomEditor
        (C.SpellStats defaultSpell)
        [ ("Spell", "spell", bind cardStatsEditorLens spellEditor (M.lens C.SpellStats setS)),
          ("Monster", "monster", bind cardStatsEditorLens monsterEditor (M.lens C.MonsterStats setM))
        ]
