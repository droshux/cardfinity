{-# LANGUAGE OverloadedStrings #-}

module Editor.View (view) where

import Data.Foldable (Foldable (toList))
import Editor.Types
import Miso qualified as M
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
  let familyInput act = const $ H.input_ [H.onChange act]
   in H.div_
        []
        [ H.button_ [H.onClick (act ToggleCardStats)] [M.text "Toggle Spell/Monster"],
          if m ^. editingSpell
            then
              spellView (act . SAction) (m ^. spellStats)
            else
              monsterView (act . MAction) (m ^. monsterStats),
          listView (act . Families) familyInput (m ^. families),
          H.input_ [H.onChange (act . SetImage)]
        ]

spellView :: SubView SpellAction SpellModel parent
spellView act m =
  H.div_
    []
    [ H.input_ [H.onChange (act . SetSpellName)],
      options (act . SetTrigger),
      listView (act . CastingConditions) conditionView (m ^. castingConditions),
      listView (act . Effects) effectsView (m ^. spellEffects)
    ]

monsterView :: SubView MonsterAction MonsterModel parent
monsterView act m =
  H.div_
    []
    [ H.input_ [H.onChange (act . SetMonsterName)],
      listView (act . SummoningConditions) conditionView (m ^. summoningConditions),
      listView (act . MonsterSpells) spellView (m ^. monsterSpells),
      H.input_
        [ P.type_ "number",
          P.min_ "0",
          H.onChange (act . SetPower . M.fromMisoString)
        ],
      H.button_ [H.onClick (act ToggleTapped)] [M.text "Toggle"]
    ]

conditionView :: SubView ConditionAction ConditionModel parent
conditionView act m =
  let count = H.input_ [P.type_ "button", P.min_ "0", H.onChange (act . CSetCount . M.fromMisoString)]
      toggle t s = H.button_ [H.onClick (act t)] [M.text s]
      stView = searchTypeView (act . CSearchTypeAction) (m ^. conditionSearchType)
      youMay = conditionView (act . SubConditionAction) (m ^. subCondition)
      choose = listView (act . SubConditionsAction) conditionView (m ^. subConditions)
   in H.span_ [] $
        concat
          [ [options (act . SetCondition)],
            [count | m ^. currentCondition `elem` [Destroy, TakeDamage, HealOpponent, Pop]],
            [toggle CToggle1 "toggle1" | m ^. currentCondition `elem` [Destroy, TakeDamage]],
            [toggle CToggle2 "Hand/Field" | (m ^. currentCondition) == Destroy],
            [stView | (m ^. currentCondition) == Destroy],
            [youMay | (m ^. currentCondition) == YouMay],
            [choose | (m ^. currentCondition) == Choose]
          ]

effectsView :: SubView EffectAction EffectModel parent
effectsView act m =
  let count = H.input_ [P.type_ "button", P.min_ "0", H.onChange (act . ESetCount . M.fromMisoString)]
      toggle t s = H.button_ [H.onClick (act t)] [M.text s]
      youMay = effectsView (act . SubEffectAction) (m ^. subEffect)
      choose = listView (act . SubEffectsAction) effectsView (m ^. subEffects)
      stView = searchTypeView (act . ESearchTypeAction) (m ^. effectSearchType)
      condView = conditionView (act . EConditionAction) (m ^. effectCondition)
   in H.span_ [] $
        concat
          [ [options (act . SetEffect)],
            [count | m ^. currentEffect `elem` [DestroyEnemy, DealDamage, Heal, Draw, Peek, Scry, Buff]],
            [toggle EToggle1 "toggle1" | m ^. currentEffect `elem` [DestroyEnemy, DealDamage, Attack, Search, Buff]],
            [toggle EToggle2 "Hand/Field" | m ^. currentEffect == DestroyEnemy],
            [youMay | (m ^. currentEffect) == Optional],
            [choose | (m ^. currentEffect) == ChooseEffect],
            [stView | m ^. currentEffect `elem` [DestroyEnemy, Play, Attach, Search]],
            [condView | (m ^. currentEffect) == AsEffect]
          ]

searchTypeView :: SubView SearchTypeAction SearchTypeModel parent
searchTypeView act m =
  let textBox = [H.input_ [H.onChange (act . SetText)] | m ^. searchTypeID `elem` [ForName, ForFamily]]
   in H.span_ [] (options (act . SetSearchType) : textBox)

listView :: (Foldable f) => (ListAction a -> DeckAction) -> SubView a m parent -> f m -> M.View parent DeckAction
listView act view =
  let add = H.button_ [H.onClick (act NewItem)] [M.text "+"]
      delete i = H.button_ [H.onClick (act $ Delete i)] [M.text "-"]
      view' i =
        H.span_ []
          . (: [delete i])
          . view (act . ItemAction i)
   in H.div_ []
        . (add :)
        . zipWith view' [0 ..]
        . toList

class (Enum a, M.ToMisoString a, M.FromMisoString a, Show a) => Options a where
  options :: (a -> DeckAction) -> M.View parent DeckAction
  options act =
    let option :: a -> M.View model action
        option a = H.option_ [P.value_ (M.toMisoString a)] [M.text $ M.toMisoString $ show a]
        opts = map option $ enumFrom $ toEnum 0
     in H.select_ [H.onChange (act . M.fromMisoString)] opts

instance Options Trigger

instance Options ConditionID

instance Options EffectID

instance Options SearchTypeID
