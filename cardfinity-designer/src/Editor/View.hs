{-# LANGUAGE OverloadedStrings #-}

module Editor.View (view) where

import Data.Foldable (Foldable (toList))
import Editor.Types
import Editor.Update (wrapLens)
import Miso qualified as M
import Miso.CSS qualified as CSS
import Miso.Html qualified as H
import Miso.Html.Property qualified as P
import Miso.Lens (Lens, (^.))
import Miso.String qualified as M
import Types (Trigger)

view :: DeckModel -> M.View parent DeckAction
view m = listView def {addButtonText = "New Card"} DeckAction cardView (m ^. deck)

type SubView a m parent = (a -> DeckAction) -> m -> M.View parent DeckAction

cardView :: SubView CardAction CardModel parent
cardView act m =
  let familyInput act f = H.input_ [H.onChange act, P.value_ f]
   in H.div_
        []
        [ H.button_ [H.onClick (act ToggleCardStats)] [M.text "Toggle Spell/Monster"],
          H.div_
            [CSS.style_ [CSS.display $ if m ^. editingSpell then "block" else "none"]]
            [spellView (act . SAction) (m ^. spellStats)],
          H.div_
            [CSS.style_ [CSS.display $ if m ^. editingSpell then "none" else "block"]]
            [monsterView (act . MAction) (m ^. monsterStats)],
          listView def {addButtonText = "+ Family"} (act . Families) familyInput (m ^. families),
          H.input_ [H.onChange (act . SetImage), P.value_ (m ^. imageUrl)],
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

spellView :: SubView SpellAction SpellModel parent
spellView act m =
  H.div_
    [ CSS.style_
        [ CSS.backgroundColor (CSS.hex "7fffff"),
          CSS.padding (CSS.em 0.5)
        ]
    ]
    [ H.input_ [H.onChange (act . SetSpellName), P.value_ (m ^. spellName)],
      options (act . SetTrigger) (m ^. spellTrigger),
      listView conditionsListSettings {addButtonText = "+ Casting Condition"} (act . CastingConditions) conditionView (m ^. castingConditions),
      listView
        def
          { backgroundColor = CSS.hex "7fff7f",
            addButtonText = "+ Effect"
          }
        (act . Effects)
        effectsView
        (m ^. spellEffects)
    ]

monsterView :: SubView MonsterAction MonsterModel parent
monsterView act m =
  H.div_
    [ CSS.style_
        [ CSS.backgroundColor (CSS.hex "ffd07f"),
          CSS.width "fit-content"
        ]
    ]
    [ H.input_ [H.onChange (act . SetMonsterName), P.value_ (m ^. monsterName)],
      listView conditionsListSettings {addButtonText = "+ Summoning Condition"} (act . SummoningConditions) conditionView (m ^. summoningConditions),
      listView def {addButtonText = "New Spell"} (act . MonsterSpells) spellView (m ^. monsterSpells),
      H.input_
        [ P.type_ "number",
          P.min_ "0",
          H.onChange (act . SetPower . M.fromMisoString),
          P.value_ (M.toMisoString $ show $ m ^. combatPower)
        ],
      H.button_ [H.onClick (act ToggleTapped)] [M.text (if m ^. entersTapped then "Begins Tapped" else "Begins Untapped")]
    ]

conditionView :: SubView ConditionAction ConditionModel parent
conditionView act m =
  let count =
        H.input_
          [ P.type_ "number",
            P.min_ "0",
            H.onChange (act . CSetCount . M.fromMisoString),
            P.value_ (M.toMisoString $ show $ m ^. conditionCount)
          ]
      toggle t s = H.button_ [H.onClick (act t)] [M.text s]
      toggle1Txt = case (m ^. currentCondition, m ^. conditionToggle) of
        (Destroy, True) -> "Banish"
        (Destroy, False) -> "Discard"
        (TakeDamage, True) -> "True Damage"
        (TakeDamage, False) -> "Damage"
        _ -> ""
   in H.span_
        []
        $ concat
          [ [options (act . SetCondition) (m ^. currentCondition)],
            [count | m ^. currentCondition `elem` [Destroy, TakeDamage, HealOpponent, Pop]],
            [toggle CToggle1 toggle1Txt | m ^. currentCondition `elem` [Destroy, TakeDamage]],
            [toggle CToggle2 (if m ^. conditionToggle2 then "Field" else "Hand") | (m ^. currentCondition) == Destroy],
            [ searchTypeView (act . CSearchTypeAction) (m ^. conditionSearchType)
              | (m ^. currentCondition) == Destroy
            ],
            [ conditionView (act . SubConditionAction) (m ^. wrapLens subCondition)
              | (m ^. currentCondition) == YouMay
            ],
            [ listView def {isNonempty = True} (act . SubConditionsAction) conditionView (m ^. wrapLens subConditions)
              | (m ^. currentCondition) == Choose
            ]
          ]

effectsView :: SubView EffectAction EffectModel parent
effectsView act m =
  let count =
        H.input_
          [ P.type_ "number",
            P.min_ "0",
            H.onChange (act . ESetCount . M.fromMisoString),
            P.value_ (M.toMisoString $ show $ m ^. effectCount)
          ]
      toggle t s = H.button_ [H.onClick (act t)] [M.text s]
      toggle1Txt = case (m ^. currentEffect, m ^. effectToggle) of
        (DestroyEnemy, True) -> "Banish"
        (DestroyEnemy, False) -> "Discard"
        (DealDamage, True) -> "True Damage"
        (DealDamage, False) -> "Damage"
        (Attack, True) -> "Piercing"
        (Attack, False) -> "Non-Piercing"
        (Search, True) -> "Search"
        (Search, False) -> "Drill"
        (Buff, True) -> "This"
        (Buff, False) -> "Other"
        _ -> ""
   in H.span_
        []
        $ concat
          [ [options (act . SetEffect) (m ^. currentEffect)],
            [ count
              | m ^. currentEffect `elem` [DestroyEnemy, DealDamage, Heal, Draw, Peek, Scry, Buff]
            ],
            [ toggle EToggle2 (if m ^. effectToggle2 then "Field" else "Hand")
              | m ^. currentEffect == DestroyEnemy
            ],
            [ toggle EToggle1 toggle1Txt
              | m ^. currentEffect `elem` [DestroyEnemy, DealDamage, Attack, Search, Buff]
            ],
            [ effectsView (act . SubEffectAction) (m ^. wrapLens subEffect)
              | (m ^. currentEffect) == Optional
            ],
            [ listView def {isNonempty = True} (act . SubEffectsAction) effectsView (m ^. wrapLens subEffects)
              | (m ^. currentEffect) == ChooseEffect
            ],
            [searchTypeView (act . ESearchTypeAction) (m ^. effectSearchType) | m ^. currentEffect `elem` [DestroyEnemy, Play, Attach, Search]],
            [conditionView (act . EConditionAction) (m ^. effectCondition) | (m ^. currentEffect) == AsEffect]
          ]

searchTypeView :: SubView SearchTypeAction SearchTypeModel parent
searchTypeView act m =
  H.span_
    []
    [ options (act . SetSearchType) (m ^. searchTypeID),
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

listView :: (Foldable f) => ListSettings -> (ListAction a -> DeckAction) -> SubView a m parent -> f m -> M.View parent DeckAction
listView settings act view items =
  let add = H.button_ [H.onClick (act NewItem)] [M.text (addButtonText settings)]
      wrap i item =
        H.span_
          [M.key_ i, CSS.style_ [CSS.display "block"]]
          [ view (act . ItemAction i) item,
            H.button_
              [ H.onClick (act $ Delete i),
                CSS.style_ [CSS.display "none" | isNonempty settings && i == 0]
              ]
              [M.text "-"]
          ]
      contents = add : zipWith wrap [0 ..] (toList items)
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
  options :: (a -> DeckAction) -> a -> M.View parent DeckAction
  options act a =
    let option :: Int -> a -> M.View model action
        option i a = H.option_ [P.value_ (M.toMisoString a), M.key_ i] [M.text $ M.toMisoString $ show a]
        opts = zipWith option [0 ..] $ enumFrom $ toEnum 0
     in H.select_ [H.onChange (act . M.fromMisoString), P.value_ (M.toMisoString a)] opts

instance Options Trigger

instance Options ConditionID

instance Options EffectID

instance Options SearchTypeID
