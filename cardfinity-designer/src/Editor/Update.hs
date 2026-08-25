module Editor.Update
  ( update,
    cardName,
    (%),
  )
where

import Control.Monad (unless)
import Data.Bifunctor (first, second)
import Editor.Types
import Miso qualified as M
import Miso.Lens hiding (set)

update :: DeckAction -> M.Effect ctx props DeckModel DeckAction
update a = do
  performUpdate a
  M.get >>= M.mailParent -- After updating, send current state to parent

performUpdate :: DeckAction -> M.Effect ctx props DeckModel DeckAction
performUpdate UpdateParent = return () -- Noop, just mail current state to parent
performUpdate NewCard = deck %= (++ [def])
performUpdate (SetCopies i n) = deck % at i %= fmap (first (const n))
performUpdate (ViewCard i) = currentCardIndex .= i
performUpdate (DeleteCard i) = deck % at i .= Nothing
performUpdate ToggleDecklist = showDecklist %= not
performUpdate (ActCard i a) = deck % at i %?= second (updateCard a)
performUpdate MoveUp = do
  current <- use currentCardIndex
  unless (current == 0) $ do
    deckList <- use deck
    let before = take (current - 1) deckList
    let after = deckList !! current : deckList !! (current - 1) : drop (current + 1) deckList
    deck .= before ++ after
    currentCardIndex .= current - 1
performUpdate MoveDown = do
  current <- use currentCardIndex
  deckList <- use deck
  unless (current + 1 == length deckList) $ do
    let before = take current deckList
    let after = deckList !! (current + 1) : deckList !! current : drop (current + 2) deckList
    deck .= before ++ after
    currentCardIndex .= current + 1

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
updateEffect (SetEffect effId) = currentEffect .~ effId
updateEffect (ESetCount n) = effectCount .~ n
updateEffect (ESetCountInt i) = effectCountInt .~ i
updateEffect EToggle1 = effectToggle %~ not
updateEffect EToggle2 = effectToggle2 %~ not
updateEffect EToggle3 = effectToggle3 %~ not
updateEffect (EffSearchType a) = effectSearchType %~ updateSearchType a
updateEffect (ESetOptional effId) = effectOptional .~ effId
updateEffect (SubEffsAction a) = subEffects %~ updateList updateEffect a
updateEffect (EffCondAction a) = effectCondition %~ updateCondition a

updateCondition :: ConditionAction -> ConditionModel -> ConditionModel
updateCondition (SetCondition condId) = currentCondition .~ condId
updateCondition (CSetCount n) = conditionCount .~ n
updateCondition CToggle1 = conditionToggle %~ not
updateCondition CToggle2 = conditionToggle2 %~ not
updateCondition CToggle3 = conditionToggle3 %~ not
updateCondition (CondSearchType a) = conditionSearchType %~ updateSearchType a
updateCondition (CSetOptional condId) = conditionOptional .~ condId
updateCondition (SubCondsAction a) = subConditions %~ updateList updateCondition a

updateSearchType :: SearchTypeAction -> SearchTypeModel -> SearchTypeModel
updateSearchType (SetSearchType stId) = searchTypeID .~ stId
updateSearchType (SetText text) = searchTypeText .~ text

updateList :: (Default am) => (aa -> am -> am) -> ListAction aa -> [am] -> [am]
updateList _ NewItem = (++ [def])
updateList _ (Delete i) = at i .~ Nothing
updateList upd (Act i a) = at i %~ fmap (upd a)

(%) :: Lens a b -> Lens b c -> Lens a c
(%) f g = M.compose g f

cardName :: Lens CardModel M.MisoString
cardName =
  let l m = if m ^. editingSpell then spellStats % spellName else monsterStats % monsterName
      get m = m ^. l m
      set m n = (l m .~ n) m
   in lens get set
