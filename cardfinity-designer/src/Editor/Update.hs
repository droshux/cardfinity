{-# LANGUAGE LambdaCase #-}

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
    updateList,
  )
where

import Control.Monad (when)
import Data.Bifunctor (first)
import Data.Foldable (Foldable (toList))
import Data.List (findIndex)
import Data.List.NonEmpty (NonEmpty ((:|)), appendList)
import Data.Maybe (fromMaybe, isNothing)
import Data.Set.Ordered (OSet, fromList, (|>))
import Debug.Trace (trace)
import Editor.Types
import GHC.Natural (naturalToInteger)
import GHC.Num (integerToInt)
import Miso qualified as M
import Miso.Lens (Lens, at, lens, (%=), (%~), (+=), (.=), (.~), (^.), _1, _2)

update :: DeckAction -> M.Effect parent DeckModel DeckAction
update NewCard = do
  deck %= ((0, def) :)
  currentCardIndex .= 0
update (SetCopies i n) = deck % wrapLens (at i) % _1 .= integerToInt (naturalToInteger n)
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

updateList :: (Default m) => ListAction a -> M.Effect parent [(Bool, m)] (ListAction a)
updateList (Delete _) =
  -- Soft delete
  M.gets (findIndex $ not . fst) >>= \case
    Nothing -> return () -- If there's nothing to delete, don't
    Just i -> M.modify $
      flip zipWith [0 ..] $
        -- Set pair with with index i to True
        \j x@(_, m) -> if j /= i then x else (True, m)
updateList NewItem =
  M.gets (findIndex fst) >>= \case
    Nothing -> M.modify $ (:) (False, def) -- If there's no deleted items: add a new one
    Just i -> M.modify $
      flip zipWith [0 ..] $
        -- Set the item at index i to new item
        \j x@(_, m) -> if j /= i then x else (False, def)

replace i mx xs =
  let x' = case mx of Just x -> [x]; Nothing -> []
   in take i xs ++ x' ++ drop (i + 1) xs

(%) :: Lens a b -> Lens b c -> Lens a c
(%) f g =
  let get = (^. g) . (^. f)
      set a c = (f .~ (g .~ c) (a ^. f)) a
   in lens get set

cardName :: Lens CardModel M.MisoString
cardName =
  let l m = if m ^. editingSpell then spellStats % spellName else monsterStats % monsterName
      get m = m ^. l m
      set m n = (l m .~ n) m
   in lens get set
