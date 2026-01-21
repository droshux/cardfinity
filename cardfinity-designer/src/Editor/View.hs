{-# LANGUAGE OverloadedStrings #-}

module Editor.View (view) where

import Data.Foldable (Foldable (toList))
import Debug.Trace (trace)
import Editor.Types
import Editor.Update
import GHC.Natural (Natural)
import Miso qualified as M
import Miso.CSS qualified as CSS
import Miso.Html qualified as H
import Miso.Html.Property qualified as P
import Miso.Lens (Lens, at, (^.))
import Miso.Lens qualified as M
import Miso.String qualified as M
import Shared qualified
import Types (Trigger)

view :: DeckModel -> M.View DeckModel DeckAction
view m =
  let item i (copies, m') =
        [ H.span_
            [ CSS.style_
                [ CSS.fontStyle $ if m' ^. cardName /= "" then "normal" else "italic"
                ]
            ]
            [M.text (if m' ^. cardName /= "" then m' ^. cardName else "no name")],
          H.button_ [H.onClick (ViewCard i)] [eyecon $ i == m ^. currentCardIndex],
          H.button_ [H.onClick (DeleteCard i)] [H.img_ [P.src_ "assets/icons/trash-2.svg"]],
          H.input_
            [ P.type_ "number",
              P.min_ "0",
              H.onChange (SetCopies i . M.fromMisoString),
              P.placeholder_ "0",
              CSS.style_
                [ CSS.minWidth $ CSS.em 1.5,
                  ("field-sizing", "content")
                ],
              P.value_ $ M.toMisoString copies
            ]
        ]
      eyecon b =
        H.img_
          [ P.src_ ("assets/icons/" <> (if b then "view" else "eye") <> ".svg")
          ]
      hideIcon b =
        H.img_
          [ P.src_ ("assets/icons/panel-left-" <> (if b then "close" else "open") <> ".svg")
          ]
      currentCard = deck % at (m ^. currentCardIndex) % wrapLens % M._2
   in H.div_
        [ CSS.style_
            [ CSS.display $ if m ^. showDecklist then "grid" else "block",
              CSS.gridTemplateColumns "auto auto 1fr",
              CSS.gap $ CSS.em 0.2
            ]
        ]
        [ H.button_ [H.onClick ToggleDecklist, CSS.style_ [CSS.display $ if null (m ^. deck) then "none" else "inline-block"]] [hideIcon $ m ^. showDecklist],
          H.button_ [H.onClick NewCard] [H.img_ [P.src_ "assets/icons/square-plus.svg"]],
          H.div_
            [ CSS.style_
                [ CSS.display $ if m ^. showDecklist then "grid" else "none",
                  CSS.rowGap $ CSS.em 0.4,
                  CSS.columnGap $ CSS.em 0.2,
                  CSS.width "fit-content",
                  CSS.gridTemplateColumns "1fr auto auto auto",
                  CSS.alignContent "start",
                  CSS.gridRowStart "2",
                  ("grid-column", "1 / span 2")
                ]
            ]
            $ concat
            $ zipWith item [0 ..] (m ^. deck),
          H.div_
            [ CSS.style_ [CSS.gridRowStart "2", CSS.gridColumnStart "3"]
            ]
            [ if null (m ^. deck) then M.text "" else "card" M.+> cardView {M.bindings = [currentCard M.<--> M._id]}
            ]
        ]

cardView :: M.Component DeckModel CardModel CardAction
cardView =
  let familyInput = M.component "" M.put $ \f -> H.input_ [H.onChange id, P.value_ f]
      snailIcon m =
        H.img_
          [ P.src_ ("assets/icons/" <> (if m ^. editingSpell then "shell" else "snail") <> ".svg")
          ]
   in M.component def updateCard $ \m ->
        H.div_
          [ CSS.style_
              [ CSS.display "flex",
                CSS.flexDirection "column",
                CSS.gap $ CSS.em 0.1,
                CSS.width "fit-content"
              ]
          ]
          [ H.button_
              [ H.onClick ToggleCardStats,
                CSS.style_ [CSS.width "fit-content"]
              ]
              [snailIcon m],
            H.div_
              [CSS.style_ [CSS.display $ if m ^. editingSpell then "block" else "none"]]
              ["spellStats" M.+> spellView {M.bindings = [spellStats M.<--> M._id]}],
            H.div_
              [CSS.style_ [CSS.display $ if m ^. editingSpell then "none" else "block"]]
              ["monsterStats" M.+> monsterView {M.bindings = [monsterStats M.<--> M._id]}],
            "families" M.+> (listView (def {addButtonText = "+ Family"}) familyInput) {M.bindings = [families M.<--> M._id]},
            H.input_ [H.onChange SetImage, CSS.style_ [CSS.width "fit-content"]],
            H.img_
              [ P.src_ (m ^. imageUrl),
                CSS.style_
                  [ CSS.width "2.5in",
                    CSS.height "2in",
                    CSS.display (if m ^. imageUrl == "" then "none" else "block"),
                    ("object-fit", "cover")
                  ]
              ]
          ]

spellView :: M.Component parent SpellModel SpellAction
spellView = M.component def updateSpell $ \m ->
  H.div_
    [ CSS.style_
        [ CSS.backgroundColor (CSS.hex "7fffff"),
          CSS.padding (CSS.em 0.5),
          CSS.width "fit-content",
          CSS.display "flex",
          CSS.flexDirection "column",
          CSS.gap (CSS.em 0.1)
        ]
    ]
    [ H.input_
        [ H.onChange SetSpellName,
          P.value_ (m ^. spellName),
          CSS.style_ [CSS.width "fit-content"]
        ],
      H.span_
        []
        [ "trigger" M.+> options {M.bindings = [spellTrigger M.<--> M._id]},
          Shared.triggerIcon (m ^. spellTrigger)
        ],
      "castingConditions" M.+> (listView (conditionsListSettings {addButtonText = "+ Casting Condition"}) conditionView) {M.bindings = [castingConditions M.<--> M._id]},
      "effects" M.+> (listView (def {backgroundColor = CSS.hex "7fff7f", addButtonText = "+ Effect"}) effectsView) {M.bindings = [spellEffects M.<--> M._id]}
    ]

monsterView :: M.Component parent MonsterModel MonsterAction
monsterView = M.component def updateMonster $ \m ->
  H.div_
    [ CSS.style_
        [ CSS.backgroundColor (CSS.hex "ffd07f"),
          CSS.width "fit-content",
          CSS.padding (CSS.em 0.5),
          CSS.display "flex",
          CSS.flexDirection "column",
          CSS.gap (CSS.em 0.1)
        ]
    ]
    [ H.input_ [H.onChange SetMonsterName, P.value_ (m ^. monsterName)],
      "summoningConditions " M.+> (listView (conditionsListSettings {addButtonText = "+ Summoning Condition"}) conditionView) {M.bindings = [summoningConditions M.<--> M._id]},
      "spells" M.+> (listView (def {addButtonText = "New Spell"}) spellView) {M.bindings = [monsterSpells M.<--> M._id]},
      H.span_
        []
        [ H.input_
            [ P.type_ "number",
              P.min_ "0",
              H.onChange (SetPower . M.fromMisoString),
              P.value_ (M.toMisoString $ show $ m ^. combatPower)
            ],
          H.button_ [H.onClick ToggleTapped] [M.text (if m ^. entersTapped then "Begins Tapped" else "Begins Untapped")]
        ]
    ]

conditionView :: M.Component parent ConditionModel ConditionAction
conditionView =
  let count =
        H.input_
          [ P.type_ "number",
            P.min_ "0",
            H.onChange (CSetCount . M.fromMisoString)
            -- P.value_ (M.toMisoString $ show $ m ^. conditionCount)
          ]
      toggle t s = H.button_ [H.onClick t] [M.text s]
      toggle1Txt m = case (m ^. currentCondition, m ^. conditionToggle) of
        (Destroy, True) -> "Banish"
        (Destroy, False) -> "Discard"
        (TakeDamage, True) -> "True Damage"
        (TakeDamage, False) -> "Damage"
        _ -> ""
      view m =
        H.span_
          []
          $ concat
            [ ["conditionId" M.+> options {M.bindings = [currentCondition M.<--> M._id]}],
              [count | m ^. currentCondition `elem` [Destroy, TakeDamage, HealOpponent, Pop]],
              [toggle CToggle1 (toggle1Txt m) | m ^. currentCondition `elem` [Destroy, TakeDamage]],
              [toggle CToggle2 (if m ^. conditionToggle2 then "Field" else "Hand") | (m ^. currentCondition) == Destroy],
              [ "searchType" M.+> searchTypeView {M.bindings = [conditionSearchType M.<--> M._id]}
                | (m ^. currentCondition) == Destroy
              ],
              [ "youMay" M.+> conditionView {M.bindings = [subCondition % wrapLens M.<--> M._id]}
                | (m ^. currentCondition) == YouMay
              ],
              [ "choose" M.+> (listView (def {isNonempty = True}) conditionView) {M.bindings = [subConditions M.<--> M._id]}
                | (m ^. currentCondition) == Choose
              ]
            ]
   in M.component def updateCondition view

effectsView :: M.Component parent EffectModel EffectAction
effectsView =
  let count =
        H.input_
          [ P.type_ "number",
            P.min_ "0",
            H.onChange (ESetCount . M.fromMisoString)
            -- P.value_ (M.toMisoString $ show $ m ^. effectCount)
          ]
      countInt =
        H.input_
          [ P.type_ "number",
            H.onChange (SetCountInt . M.fromMisoString)
            -- P.value_ (M.toMisoString $ show $ m ^. effectCountInt)
          ]
      toggle t s = H.button_ [H.onClick t] [M.text s]
      toggle1Txt m = case (m ^. currentEffect, m ^. effectToggle) of
        (DestroyEnemy, True) -> "Banish"
        (DestroyEnemy, False) -> "Discard"
        (DealDamage, True) -> "True Damage"
        (DealDamage, False) -> "Damage"
        (Attack, True) -> "Piercing"
        (Attack, False) -> "Non-Piercing"
        (Search, True) -> "Search"
        (Search, False) -> "Drill"
        (Buff, True) -> "Other"
        (Buff, False) -> "This"
        _ -> ""
      view m =
        H.span_
          []
          $ concat
            [ ["effectId" M.+> options {M.bindings = [currentEffect M.<--> M._id]}],
              [ count
                | m ^. currentEffect `elem` [DestroyEnemy, DealDamage, Heal, Draw, Peek, Scry]
              ],
              [ countInt
                | m ^. currentEffect == Buff
              ],
              [ toggle EToggle2 (if m ^. effectToggle2 then "Field" else "Hand")
                | m ^. currentEffect == DestroyEnemy
              ],
              [ toggle EToggle1 (toggle1Txt m)
                | m ^. currentEffect `elem` [DestroyEnemy, DealDamage, Attack, Search, Buff]
              ],
              [ "optional" M.+> effectsView {M.bindings = [subEffect % wrapLens M.<--> M._id]}
                | (m ^. currentEffect) == Optional
              ],
              ["choose" M.+> (listView (def {isNonempty = True}) effectsView) {M.bindings = [subEffects M.<--> M._id]} | m ^. currentEffect == ChooseEffect],
              ["searchType" M.+> searchTypeView {M.bindings = [effectSearchType M.<--> M._id]} | m ^. currentEffect `elem` [DestroyEnemy, Play, Attach, Search]],
              ["asEffect" M.+> conditionView {M.bindings = [effectCondition M.<--> M._id]} | (m ^. currentEffect) == AsEffect]
            ]
   in M.component def updateEffect view

searchTypeView :: M.Component parent SearchTypeModel SearchTypeAction
searchTypeView = M.component def updateSearchType $ \m ->
  H.span_
    []
    [ "opt" M.+> options {M.bindings = [searchTypeID M.<--> M._id]}, -- options (act . SetSearchType) (m ^. searchTypeID),
      H.input_
        [ H.onChange SetText,
          P.value_ (m ^. searchTypeText),
          CSS.style_ [CSS.display $ if m ^. searchTypeID `elem` [ForName, ForFamily] then "inline" else "none"]
        ]
    ]

data ListSettings = ListSettings
  { isNonempty :: Bool,
    addButtonText :: M.MisoString,
    backgroundColor :: CSS.Color
  }

instance Default ListSettings where
  def =
    ListSettings
      { isNonempty = False,
        addButtonText = "+",
        backgroundColor = CSS.transparent
      }

conditionsListSettings = def {backgroundColor = CSS.hex "ff7f7f"}

listView :: (Eq m, Default m) => ListSettings -> M.Component [m] m a -> M.Component parent [m] (ListAction a)
listView settings child = M.component [] update view
  where
    update NewItem = M.modify (++ [def])
    update (Delete i) = do
      M.io_ $ M.consoleLog ("Deleting " <> M.toMisoString i)
      M.modify $ \xs -> take i xs ++ drop (i + 1) xs
    view xs =
      let add = H.button_ [H.onClick NewItem] [M.text (addButtonText settings)]
          wrap i item =
            H.span_
              [M.key_ i, CSS.style_ [CSS.display "block"]]
              [ M.toMisoString i M.+> child {M.bindings = [at i % wrapLens M.<--> M._id]},
                H.button_
                  [ H.onClick $ trace ("Deleting " ++ show i) (Delete i),
                    CSS.style_ [CSS.display "none" | isNonempty settings && i == 0]
                  ]
                  [M.text "-"]
              ]
          contents = add : zipWith wrap [0 ..] xs
       in H.div_
            [ CSS.style_
                [ CSS.border "thin black solid",
                  CSS.width "fit-content",
                  CSS.padding (CSS.em 0.3),
                  CSS.backgroundColor (backgroundColor settings)
                ]
            ]
            contents

class (Enum a, M.ToMisoString a, M.FromMisoString a, Show a) => Options a where
  options :: M.Component parent a a
  options =
    let option :: Int -> a -> M.View model action
        option i a = H.option_ [P.value_ (M.toMisoString a), M.key_ i] [M.text $ M.toMisoString $ show a]
        opts = zipWith option [0 ..] $ enumFrom $ toEnum 0
        view a = H.select_ [H.onChange M.fromMisoString, P.value_ $ M.toMisoString a] opts
     in M.component (toEnum 0) M.put view

instance Options Trigger

instance Options ConditionID

instance Options EffectID

instance Options SearchTypeID
