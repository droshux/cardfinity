module Editor.Update
  ( update,
    wrapLens,
    cardName,
    (%),
    updateSearchType,
    updateEffect,
    updateCondition,
    updateMonster,
    updateSpell,
    updateCard,
  )
where

import Control.Monad (when)
import Data.Bifunctor (first)
import Data.Foldable (Foldable (toList))
import Data.List (findIndex)
import Data.List.NonEmpty (NonEmpty ((:|)), appendList)
import Data.Maybe (fromMaybe, isNothing)
import Data.Set.Ordered (OSet, fromList, (|>))
import Editor.Types
import GHC.Natural (naturalToInteger)
import GHC.Num (integerToInt)
import Miso qualified as M
import Miso.Lens (Lens, at, lens, (%=), (%~), (+=), (.=), (.~), (?~), (^.), _1, _2)

update :: DeckAction -> M.Effect parent DeckModel DeckAction
update NewCard = do
  newIndex <- M.gets (length . (^. deck))
  deck %= (++ [(0, def)])
  currentCardIndex .= newIndex
update (SetCopies i n) = deck % at i % wrapLens % _1 .= integerToInt (naturalToInteger n)
update (ViewCard i) = currentCardIndex .= i
update (DeleteCard i) = deck % at i .= Nothing
update ToggleDecklist = showDecklist %= not

updateCard :: CardAction -> M.Effect parent CardModel CardAction
updateCard ToggleCardStats = editingSpell %= not
updateCard (SetImage s) = imageUrl .= s

updateMonster :: MonsterAction -> M.Effect parent MonsterModel MonsterAction
updateMonster (SetMonsterName n) = monsterName .= n
updateMonster (SetPower p) = combatPower .= p
updateMonster ToggleTapped = entersTapped %= not

updateSpell :: SpellAction -> M.Effect parent SpellModel SpellAction
updateSpell (SetSpellName n) = spellName .= n
updateSpell (SetTrigger t) = spellTrigger .= t

updateEffect :: EffectAction -> M.Effect parent EffectModel EffectAction
updateEffect (SetEffect id) = currentEffect .= id
updateEffect (ESetCount n) = effectCount .= n
updateEffect (SetCountInt i) = effectCountInt .= i
updateEffect EToggle1 = effectToggle %= not
updateEffect EToggle2 = effectToggle2 %= not

updateCondition :: ConditionAction -> M.Effect parent ConditionModel ConditionAction
updateCondition (SetCondition id) = currentCondition .= id
updateCondition (CSetCount n) = conditionCount .= n
updateCondition CToggle1 = conditionToggle %= not
updateCondition CToggle2 = conditionToggle2 %= not

updateSearchType :: SearchTypeAction -> M.Effect parent SearchTypeModel SearchTypeAction
updateSearchType (SetSearchType id) = searchTypeID .= id
updateSearchType (SetText t) = searchTypeText .= t

wrapLens :: (Default a) => Lens (Maybe a) a
wrapLens = lens (fromMaybe def) (flip (fmap . const))

(%) :: Lens a b -> Lens b c -> Lens a c
(%) f g = M.compose g f

cardName :: Lens CardModel M.MisoString
cardName =
  let l m = if m ^. editingSpell then spellStats % spellName else monsterStats % monsterName
      get m = m ^. l m
      set m n = (l m .~ n) m
   in lens get set
