module Editor.Update
  ( update,
    cardName,
    (%),
  )
where

import Data.Bifunctor (first, second)
import Data.Maybe (fromMaybe)
import Editor.Types
import Miso qualified as M
import Miso.Lens

update :: DeckAction -> M.Effect parent DeckModel DeckAction
update NewCard = deck %= (++ [def])
update (SetCopies i n) = deck % at i %= fmap (first (+ 1))
update (ViewCard i) = currentCardIndex .= i
update (DeleteCard i) = deck % at i .= Nothing
update ToggleDecklist = showDecklist %= not
update (ActCard i a) = deck % at i %?= second (updateCard a)

updateCard :: CardAction -> CardModel -> CardModel
updateCard ToggleCardStats = editingSpell %~ not
updateCard (SetImage url) = imageUrl .~ url
updateCard (ActSpell a) = spellStats %~ updateSpell a
updateCard (ActMonster a) = monsterStats %~ updateMonster a
updateCard (ActFamilies a) = families %~ updateList const a

updateMonster :: MonsterAction -> MonsterModel -> MonsterModel
updateMonster (SetMonsterName name) = monsterName .~ name
updateMonster (SetPower p) = combatPower .~ p
updateMonster ToggleTapped = entersTapped %~ not
updateMonster (ActSummonCond a) = summoningConditions %~ updateList updateCondition a
updateMonster (ActSpells a) = monsterSpells %~ updateList updateSpell a

updateSpell :: SpellAction -> SpellModel -> SpellModel
updateSpell (SetSpellName name) = spellName .~ name
updateSpell (SetTrigger t) = spellTrigger .~ t
updateSpell (ActCond a) = castingConditions %~ updateList updateCondition a
updateSpell (ActEff a) = spellEffects %~ updateList updateEffect a

updateEffect :: EffectAction -> EffectModel -> EffectModel
updateEffect (SetEffect id) = currentEffect .~ id
updateEffect (ESetCount n) = effectCount .~ n
updateEffect (ESetCountInt i) = effectCountInt .~ i
updateEffect EToggle1 = effectToggle %~ not
updateEffect EToggle2 = effectToggle2 %~ not
updateEffect (EffSearchType a) = effectSearchType %~ updateSearchType a
updateEffect (SubEffAction a) = subEffect %~ fmap (updateEffect a)
updateEffect (SubEffsAction a) = subEffects %~ updateList updateEffect a
updateEffect (EffCondAction a) = effectCondition %~ updateCondition a

updateCondition :: ConditionAction -> ConditionModel -> ConditionModel
updateCondition (SetCondition id) = currentCondition .~ id
updateCondition (CSetCount n) = conditionCount .~ n
updateCondition CToggle1 = conditionToggle %~ not
updateCondition CToggle2 = conditionToggle2 %~ not
updateCondition (CondSearchType a) = conditionSearchType %~ updateSearchType a
updateCondition (SubCondAction a) = subCondition %~ fmap (updateCondition a)
updateCondition (SubCondsAction a) = subConditions %~ updateList updateCondition a

updateSearchType :: SearchTypeAction -> SearchTypeModel -> SearchTypeModel
updateSearchType (SetSearchType id) = searchTypeID .~ id
updateSearchType (SetText text) = searchTypeText .~ text

updateList :: (Default am) => (aa -> am -> am) -> ListAction aa -> [am] -> [am]
updateList update NewItem = (++ [def])
updateList update (Delete i) = at i .~ Nothing
updateList update (Act i a) = at i %~ fmap (update a)

(%) :: Lens a b -> Lens b c -> Lens a c
(%) f g = M.compose g f

cardName :: Lens CardModel M.MisoString
cardName =
  let l m = if m ^. editingSpell then spellStats % spellName else monsterStats % monsterName
      get m = m ^. l m
      set m n = (l m .~ n) m
   in lens get set
