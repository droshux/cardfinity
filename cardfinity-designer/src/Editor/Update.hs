module Editor.Update (update) where

import Data.Foldable (Foldable (toList))
import Data.List.NonEmpty (NonEmpty ((:|)), cons)
import Data.Set.Ordered (OSet, fromList, (<|))
import Editor.Types
import Miso qualified as M
import Miso.Lens (Lens, lens, (%=), (%~), (.=), (.~), (^.))

type Effect parent = M.Effect parent DeckModel DeckAction

update :: DeckAction -> Effect parent
update (DeckAction la) = listHelper deck updateCard la

updateCard :: SubUpdate CardAction CardModel parent
updateCard card (Families action) = osetHelper (card % families) (.=) action
updateCard card ToggleCardStats = card % editingSpell %= not
updateCard card (SetImage s) = card % imageUrl .= s
updateCard card (MAction action) = updateMonster (card % monsterStats) action
updateCard card (SAction action) = updateSpell (card % spellStats) action

updateMonster :: SubUpdate MonsterAction MonsterModel parent
updateMonster m (SetMonsterName n) = m % monsterName .= n
updateMonster m (MonsterSpells action) = listHelper (m % monsterSpells) updateSpell action
updateMonster m (SummoningConditions action) = osetHelper (m % summoningConditions) updateCondition action
updateMonster m (SetPower p) = m % combatPower .= p
updateMonster m ToggleTapped = m % entersTapped %= not

updateSpell :: SubUpdate SpellAction SpellModel parent
updateSpell s (SetSpellName n) = s % spellName .= n
updateSpell s (SetTrigger t) = s % spellTrigger .= t
updateSpell s (Effects action) = listHelper (s % spellEffects) updateEffect action
updateSpell s (CastingConditions action) = osetHelper (s % castingConditions) updateCondition action

updateEffect :: SubUpdate EffectAction EffectModel parent
updateEffect e (SetEffect id) = e % currentEffect .= id
updateEffect e (ESetCount n) = e % effectCount .= n
updateEffect e EToggle1 = e % effectToggle %= not
updateEffect e EToggle2 = e % effectToggle2 %= not
updateEffect e (SubEffectAction action) = updateEffect (e % subEffect) action
updateEffect e (SubEffectsAction action) = nelistHelper (e % subEffects) updateEffect action
updateEffect e (ESearchTypeAction action) = updateSearchType (e % effectSearchType) action
updateEffect e (EConditionAction action) = updateCondition (e % effectCondition) action

updateCondition :: SubUpdate ConditionAction ConditionModel parent
updateCondition c (SetCondition id) = c % currentCondition .= id
updateCondition c (CSetCount n) = c % conditionCount .= n
updateCondition c CToggle1 = c % conditionToggle %= not
updateCondition c CToggle2 = c % conditionToggle2 %= not
updateCondition c (CSearchTypeAction action) = updateSearchType (c % conditionSearchType) action
updateCondition c (SubConditionAction action) = updateCondition (c % subCondition) action
updateCondition c (SubConditionsAction action) = nelistHelper (c % subConditions) updateCondition action

updateSearchType :: SubUpdate SearchTypeAction SearchTypeModel parent
updateSearchType st (SetSearchType id) = st % searchTypeID .= id
updateSearchType st (SetText t) = st % searchTypeText .= t

type SubUpdate a m parent = Lens DeckModel m -> a -> Effect parent

listHelper ::
  (Default m) =>
  Lens DeckModel [m] ->
  SubUpdate a m parent ->
  ListAction a ->
  Effect parent
listHelper l u NewItem = l %= (def :)
listHelper l u (Delete i) = l %= replace i Nothing
listHelper l u (ItemAction i a) = u (focus l i) a
  where
    focus l i =
      let get m = (m ^. l) !! i; set x = l %~ replace i (Just x)
       in lens get (flip set)

osetHelper ::
  (Default m, Ord m) =>
  Lens DeckModel (OSet m) ->
  SubUpdate a m parent ->
  ListAction a ->
  Effect parent
osetHelper l u NewItem = l %= (def <|)
osetHelper l u (Delete i) = l %= (fromList . replace i Nothing . toList)
osetHelper l u (ItemAction i a) = u (focus l i) a
  where
    focus l i =
      let get m = toList (m ^. l) !! i
          set x = l %~ fromList . replace i (Just x) . toList
       in lens get (flip set)

nelistHelper ::
  (Default m) =>
  Lens DeckModel (NonEmpty m) ->
  SubUpdate a m parent ->
  ListAction a ->
  Effect parent
nelistHelper l u NewItem = l %= cons def
nelistHelper l u (Delete i) = l %= replaceNE i Nothing
nelistHelper l u (ItemAction i a) = u (focus l i) a
  where
    focus l i =
      let get m = case (i, m ^. l) of
            (0, x :| _) -> x
            (i, _ :| tail) -> tail !! (i - 1)
          set x = l %~ replaceNE i (Just x)
       in lens get (flip set)

replace i mx xs =
  let x' = case mx of Just x -> [x]; Nothing -> []
   in take i xs ++ x' ++ drop (i + 1) xs

replaceNE 0 Nothing xs@(_ :| []) = xs -- Illegal Operation => Noop
replaceNE 0 Nothing (_ :| (h : tail)) = h :| tail
replaceNE 0 (Just x) (_ :| tail) = x :| tail
replaceNE i mx (h :| tail) = h :| replace (i - 1) mx tail

(%) :: Lens a b -> Lens b c -> Lens a c
(%) f g =
  let get = (^. g) . (^. f)
      set a c = (f .~ (g .~ c) (a ^. f)) a
   in lens get set
