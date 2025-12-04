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
view m = listView DeckAction cardView (m ^. deck)

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
          listView (act . Families) familyInput (m ^. families),
          H.input_ [H.onChange (act . SetImage), P.value_ (m ^. imageUrl)]
        ]

spellView :: SubView SpellAction SpellModel parent
spellView act m =
  H.div_
    []
    [ H.input_ [H.onChange (act . SetSpellName), P.value_ (m ^. spellName)],
      options (act . SetTrigger) (m ^. spellTrigger),
      listView (act . CastingConditions) conditionView (m ^. castingConditions),
      listView (act . Effects) effectsView (m ^. spellEffects)
    ]

monsterView :: SubView MonsterAction MonsterModel parent
monsterView act m =
  H.div_
    []
    [ H.input_ [H.onChange (act . SetMonsterName), P.value_ (m ^. monsterName)],
      listView (act . SummoningConditions) conditionView (m ^. summoningConditions),
      listView (act . MonsterSpells) spellView (m ^. monsterSpells),
      H.input_
        [ P.type_ "number",
          P.min_ "0",
          H.onChange (act . SetPower . M.fromMisoString),
          P.value_ (M.toMisoString $ show $ m ^. combatPower)
        ],
      H.button_ [H.onClick (act ToggleTapped)] [M.text "Toggle"]
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
   in H.span_ [] $
        concat
          [ [options (act . SetCondition) (m ^. currentCondition)],
            [count | m ^. currentCondition `elem` [Destroy, TakeDamage, HealOpponent, Pop]],
            [toggle CToggle1 "toggle1" | m ^. currentCondition `elem` [Destroy, TakeDamage]],
            [toggle CToggle2 "Hand/Field" | (m ^. currentCondition) == Destroy],
            [ searchTypeView (act . CSearchTypeAction) (m ^. conditionSearchType)
              | (m ^. currentCondition) == Destroy
            ],
            [ conditionView (act . SubConditionAction) (m ^. wrapLens subCondition)
              | (m ^. currentCondition) == YouMay
            ],
            [ listView (act . SubConditionsAction) conditionView (m ^. wrapLens subConditions)
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
   in H.span_ [] $
        concat
          [ [options (act . SetEffect) (m ^. currentEffect)],
            [ count
              | m ^. currentEffect `elem` [DestroyEnemy, DealDamage, Heal, Draw, Peek, Scry, Buff]
            ],
            [ toggle EToggle1 "toggle1"
              | m ^. currentEffect `elem` [DestroyEnemy, DealDamage, Attack, Search, Buff]
            ],
            [ toggle EToggle2 "Hand/Field"
              | m ^. currentEffect == DestroyEnemy
            ],
            [ effectsView (act . SubEffectAction) (m ^. wrapLens subEffect)
              | (m ^. currentEffect) == Optional
            ],
            [ listView (act . SubEffectsAction) effectsView (m ^. wrapLens subEffects)
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
          CSS.style_ [CSS.display $ if m ^. searchTypeID `elem` [ForName, ForFamily] then "block" else "none"]
        ]
    ]

listView :: (Foldable f) => (ListAction a -> DeckAction) -> SubView a m parent -> f m -> M.View parent DeckAction
listView act view items =
  let add = H.button_ [H.onClick (act NewItem)] [M.text "+"]
      wrap i item =
        H.span_
          [M.key_ i]
          [ view (act . ItemAction i) item,
            H.button_ [H.onClick (act $ Delete i)] [M.text "-"]
          ]
   in H.div_ [] $ add : zipWith wrap [0 ..] (toList items)

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
