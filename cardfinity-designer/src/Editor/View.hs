{-# LANGUAGE OverloadedStrings #-}

module Editor.View (view) where

import Data.Bifunctor (second)
import Data.Foldable (Foldable (toList))
import Data.Maybe (maybeToList)
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

view :: ctx -> props -> DeckModel -> M.View ctx DeckAction
view _ _ m =
  let index = m ^. currentCardIndex
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
            [ CSS.style_
                [ CSS.gridRowStart "2",
                  CSS.gridColumnStart "3",
                  CSS.display "flex",
                  CSS.flexDirection "column",
                  CSS.gap $ CSS.em 0.1,
                  CSS.width "fit-content"
                ]
            ]
            [cardView (ActCard index) (snd $ (m ^. deck) !! index)]
        ]
  where
    item i (copies, m') =
      [ H.span_
          [ CSS.style_
              [ CSS.fontStyle $ if m' ^. cardName /= def then "normal" else "italic"
              ]
          ]
          [M.text (if m' ^. cardName /= def then M.toMisoString $ m' ^. cardName else "no name")],
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

cardView :: (CardAction -> DeckAction) -> CardModel -> M.View ctx DeckAction
cardView act m =
  H.div_
    [ CSS.style_
        [ CSS.gridRowStart "2",
          CSS.gridColumnStart "3",
          CSS.display "flex",
          CSS.flexDirection "column",
          CSS.gap $ CSS.em 0.1,
          CSS.width "fit-content"
        ]
    ]
    [ H.button_ [H.onClick (act ToggleCardStats), CSS.style_ [CSS.width "fit-content"]] [snailIcon m],
      H.div_ [CSS.style_ [CSS.display $ if m ^. editingSpell then "block" else "none"]] [spellView (act . ActSpell) (m ^. spellStats)],
      H.div_ [CSS.style_ [CSS.display $ if m ^. editingSpell then "none" else "block"]] [monsterView (act . ActMonster) (m ^. monsterStats)],
      listView (def {addButtonText = "+ Family"}) (act . ActFamilies) familyInput (m ^. families),
      H.input_ [H.onChange (act . SetImage), P.value_ (m ^. imageUrl), CSS.style_ [CSS.width "fit-content"]],
      H.img_ [P.src_ (m ^. imageUrl), CSS.style_ [CSS.width "2.5in", CSS.height "2in", CSS.display (if m ^. imageUrl == def then "none" else "block"), ("object-fit", "cover")]]
    ]
  where
    snailIcon m =
      H.img_
        [ P.src_ ("assets/icons/" <> (if m ^. editingSpell then "shell" else "snail") <> ".svg")
        ]
    familyInput set f = H.input_ [H.onChange set, P.value_ f]

spellView :: (SpellAction -> DeckAction) -> SpellModel -> M.View ctx DeckAction
spellView act m =
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
        [ H.onChange (act . SetSpellName),
          P.value_ (m ^. spellName),
          CSS.style_ [CSS.width "fit-content"]
        ],
      H.span_
        []
        [ options (const True) (act . SetTrigger) (m ^. spellTrigger),
          Shared.triggerIcon (m ^. spellTrigger),
          listView (conditionsListSettings {addButtonText = "+Casting Condition"}) (act . ActCond) conditionView (m ^. castingConditions),
          listView (def {backgroundColor = CSS.hex "7fff7f", addButtonText = "+ Effect"}) (act . ActEff) effectView (m ^. spellEffects)
        ]
    ]

monsterView :: (MonsterAction -> DeckAction) -> MonsterModel -> M.View ctx DeckAction
monsterView act m =
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
    [ H.input_ [H.onChange (act . SetMonsterName), P.value_ (m ^. monsterName)],
      listView (conditionsListSettings {addButtonText = "+ Summoning Condition"}) (act . ActSummonCond) conditionView (m ^. summoningConditions),
      listView (def {addButtonText = "New Spell"}) (act . ActSpells) spellView (m ^. monsterSpells),
      H.span_
        []
        [ H.input_
            [ P.type_ "number",
              P.min_ "0",
              H.onChange (act . SetPower . M.fromMisoString),
              P.value_ (M.toMisoString $ show $ m ^. combatPower)
            ],
          H.button_ [H.onClick (act ToggleTapped)] [M.text (if m ^. entersTapped then "Begins Tapped" else "Begins Untapped")]
        ]
    ]

conditionView :: (ConditionAction -> DeckAction) -> ConditionModel -> M.View ctx DeckAction
conditionView act m =
  H.span_
    []
    $ concat
      [ [options (const True) (act . SetCondition) (m ^. currentCondition)],
        [ options (/= YouMay) (act . CSetOptional) (m ^. conditionOptional)
        | m ^. currentCondition == YouMay
        ],
        [ count
        | m ^. currentCondition' `elem` [Destroy, TakeDamage, HealOpponent, Pop]
        ],
        [ toggle (act CToggle1) (toggle1Txt m)
        | m ^. currentCondition' `elem` [Destroy, TakeDamage]
        ],
        [ toggle (act CToggle2) (if m ^. conditionToggle2 then "Field" else "Hand")
        | m ^. currentCondition' == Destroy
        ],
        [ searchTypeView (act . CondSearchType) (m ^. conditionSearchType)
        | m ^. currentCondition' == Destroy
        ],
        [ listView (def {isNonempty = True}) (act . SubCondsAction) conditionView (m ^. subConditions)
        | m ^. currentCondition' == Choose
        ]
      ]
  where
    count =
      H.input_
        [ P.type_ "number",
          P.min_ "0",
          H.onChange (act . CSetCount . M.fromMisoString),
          P.value_ (M.toMisoString $ show $ m ^. conditionCount)
        ]
    toggle t s = H.button_ [H.onClick t] [M.text s]
    toggle1Txt m = case (m ^. currentCondition', m ^. conditionToggle) of
      (Destroy, True) -> "Banish"
      (Destroy, False) -> "Discard"
      (TakeDamage, True) -> "True Damage"
      (TakeDamage, False) -> "Damage"
      _ -> ""

effectView :: (EffectAction -> DeckAction) -> EffectModel -> M.View ctx DeckAction
effectView act m =
  H.span_
    []
    $ concat
      [ [options (const True) (act . SetEffect) (m ^. currentEffect)],
        [ options (/= Optional) (act . ESetOptional) (m ^. effectOptional)
        | m ^. currentEffect == Optional
        ],
        [ count
        | m ^. currentEffect' `elem` [DestroyEnemy, DealDamage, Heal, Draw, Peek, Scry]
        ],
        [ countInt
        | m ^. currentEffect' == Buff
        ],
        [ toggle (act EToggle2) (if m ^. effectToggle2 then "Field" else "Hand")
        | m ^. currentEffect' == DestroyEnemy
        ],
        [ toggle (act EToggle1) (toggle1Txt m)
        | m ^. currentEffect' `elem` [DestroyEnemy, DealDamage, Attack, Search, Buff]
        ],
        [ listView (def {isNonempty = True}) (act . SubEffsAction) effectView (m ^. subEffects)
        | m ^. currentEffect' == ChooseEffect
        ],
        [ searchTypeView (act . EffSearchType) (m ^. effectSearchType)
        | m ^. currentEffect' `elem` [DestroyEnemy, Play, Attach, Search]
        ],
        [ conditionView (act . EffCondAction) (m ^. effectCondition)
        | m ^. currentEffect' == AsEffect
        ]
      ]
  where
    count =
      H.input_
        [ P.type_ "number",
          P.min_ "0",
          H.onChange (act . ESetCount . M.fromMisoString),
          P.value_ (M.toMisoString $ show $ m ^. effectCount)
        ]
    countInt =
      H.input_
        [ P.type_ "number",
          H.onChange (act . ESetCountInt . M.fromMisoString)
          -- P.value_ (M.toMisoString $ show $ m ^. effectCountInt)
        ]
    toggle t s = H.button_ [H.onClick t] [M.text s]
    toggle1Txt m = case (m ^. currentEffect', m ^. effectToggle) of
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

searchTypeView :: (SearchTypeAction -> DeckAction) -> SearchTypeModel -> M.View ctx DeckAction
searchTypeView act m =
  H.span_
    []
    [ options (const True) (act . SetSearchType) (m ^. searchTypeID),
      H.input_
        [ H.onChange (act . SetText),
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

listView :: (Eq m, Default m) => ListSettings -> (ListAction a -> DeckAction) -> ((a -> DeckAction) -> m -> M.View ctx DeckAction) -> [m] -> M.View ctx DeckAction
listView settings promote view xs =
  H.div_
    [ CSS.style_
        [ CSS.border "thin black solid",
          CSS.width "fit-content",
          CSS.padding (CSS.em 0.3),
          CSS.backgroundColor (backgroundColor settings)
        ]
    ]
    contents
  where
    contents = add : zipWith wrap [0 ..] xs
    add = H.button_ [H.onClick (promote NewItem)] [M.text (addButtonText settings)]
    wrap i item =
      H.span_
        [M.key_ i, CSS.style_ [CSS.display "block"]]
        [ view (promote . Act i) item,
          H.button_
            [ H.onClick $ trace ("Deleting " ++ show i) (promote $ Delete i),
              CSS.style_ [CSS.display "none" | isNonempty settings && i == 0]
            ]
            [M.text "-"]
        ]

class (Enum a, M.ToMisoString a, M.FromMisoString a, Show a) => Options a where
  options :: (a -> Bool) -> (a -> DeckAction) -> a -> M.View ctx DeckAction
  options f act a =
    let option :: Int -> a -> M.View model action
        option i a = H.option_ [P.value_ (M.toMisoString a), M.key_ i] [M.text $ M.toMisoString $ show a]
        opts = zipWith option [0 ..] $ filter f $ enumFrom $ toEnum 0
     in H.select_ [H.onChange (act . M.fromMisoString), P.value_ $ M.toMisoString a] opts

instance Options Trigger

instance Options ConditionID

instance Options EffectID

instance Options SearchTypeID
