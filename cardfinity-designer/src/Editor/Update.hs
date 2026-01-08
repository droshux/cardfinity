module Editor.Update
  ( update,
    wrapLens,
    cardName,
    (%),
    focus,
    updateSearchType,
    lensNonEmpty,
    lensOset,
    updateEffect,
    updateCondition,
    updateMonster,
    updateSpell,
    updateCard,
  )
where

import Control.Monad (when)
import Data.Foldable (Foldable (toList))
import Data.List.NonEmpty (NonEmpty ((:|)), appendList)
import Data.Maybe (fromMaybe, isNothing)
import Data.Set.Ordered (OSet, fromList, (|>))
import Editor.Types
import GHC.Natural (naturalToInteger)
import GHC.Num (integerToInt)
import Miso qualified as M
import Miso.Lens (Lens, lens, (%=), (%~), (+=), (.=), (.~), (^.), _1, _2)

update :: DeckAction -> M.Effect parent DeckModel DeckAction
update NewCard = do
  deck %= ((0, def) :)
  currentCardIndex .= 0
update (SetCopies i n) = focus deck i % _1 .= integerToInt (naturalToInteger n)
update (ViewCard i) = currentCardIndex .= i
update (DeleteCard i) = do
  current <- M.gets (^. currentCardIndex)
  when (current == i) $ do
    len <- M.gets (length . (^. deck))
    let delta = if i + 1 < len then 0 else -1
    currentCardIndex += delta
  deck %= replace i Nothing
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

wrapLens :: (Default b) => Lens a (Maybe b) -> Lens a b
wrapLens l =
  let get = fromMaybe def . (^. l)
      set = (l %~) . fmap . const
   in lens get (flip set)

lensNonEmpty :: (Default b) => Lens a [b] -> Lens a (NonEmpty b)
lensNonEmpty l = lens (h . flip (^.) l) (flip $ (l .~) . toList)
  where
    h [] = def :| []
    h (x : xs) = x :| xs

lensOset :: (Ord b) => Lens a [b] -> Lens a (OSet b)
lensOset l = lens (fromList . flip (^.) l) (flip $ (l .~) . toList)

updateEffect :: EffectAction -> M.Effect parent EffectModel EffectAction
updateEffect (SetEffect id) = do
  currentEffect .= id
  noChild <- M.gets $ isNothing . (^. subEffect)
  when (id == Optional && noChild) $ subEffect .= Just def
  noChildren <- M.gets $ isNothing . (^. subEffects)
  when (id == ChooseEffect && noChildren) $ subEffects .= Just def
updateEffect (ESetCount n) = effectCount .= n
updateEffect (SetCountInt i) = effectCountInt .= i
updateEffect EToggle1 = effectToggle %= not
updateEffect EToggle2 = effectToggle2 %= not

updateCondition :: ConditionAction -> M.Effect parent ConditionModel ConditionAction
updateCondition (SetCondition id) = do
  currentCondition .= id
  noChild <- M.gets $ isNothing . (^. subCondition)
  when (id == YouMay && noChild) $ subCondition .= Just def
  noChildren <- M.gets $ isNothing . (^. subConditions)
  when (id == Choose) $ subConditions .= Just def
updateCondition (CSetCount n) = conditionCount .= n
updateCondition CToggle1 = conditionToggle %= not
updateCondition CToggle2 = conditionToggle2 %= not

updateSearchType :: SearchTypeAction -> M.Effect parent SearchTypeModel SearchTypeAction
updateSearchType (SetSearchType id) = searchTypeID .= id
updateSearchType (SetText t) = searchTypeText .= t

replace i mx xs =
  let x' = case mx of Just x -> [x]; Nothing -> []
   in take i xs ++ x' ++ drop (i + 1) xs

(%) :: Lens a b -> Lens b c -> Lens a c
(%) f g =
  let get = (^. g) . (^. f)
      set a c = (f .~ (g .~ c) (a ^. f)) a
   in lens get set

focus l i =
  let get m = (m ^. l) !! i; set x = l %~ replace i (Just x)
   in lens get (flip set)

cardName :: Lens CardModel M.MisoString
cardName =
  let l m = if m ^. editingSpell then spellStats % spellName else monsterStats % monsterName
      get m = m ^. l m
      set m n = (l m .~ n) m
   in lens get set
