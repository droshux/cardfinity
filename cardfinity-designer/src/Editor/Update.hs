module Editor.Update (update, wrapLens, cardName, (%), focus) where

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

type Effect parent = M.Effect parent DeckModel DeckAction

update :: DeckAction -> Effect parent
update NewCard = do
  deck %= ((0, def) :)
  currentCardIndex .= 0
update (SetCopies i n) = focus deck i % _1 .= integerToInt (naturalToInteger n)
update (DCardAction i act) = flip updateCard act $ focus deck i % _2
update (ViewCard i) = currentCardIndex .= i
update (DeleteCard i) = do
  current <- M.gets (^. currentCardIndex)
  when (current == i) $ do
    len <- M.gets (length . (^. deck))
    let delta = if i + 1 < len then 0 else -1
    currentCardIndex += delta
  deck %= replace i Nothing
update ToggleDecklist = showDecklist %= not

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

wrapLens :: (Default b) => Lens a (Maybe b) -> Lens a b
wrapLens l =
  let get = fromMaybe def . (^. l)
      set = (l %~) . fmap . const
   in lens get (flip set)

updateEffect :: SubUpdate EffectAction EffectModel parent
updateEffect e (SetEffect id) = do
  e % currentEffect .= id
  noChild <- M.gets $ isNothing . (^. e % subEffect)
  when (id == Optional && noChild) $ e % subEffect .= Just def
  noChildren <- M.gets $ isNothing . (^. e % subEffects)
  when (id == ChooseEffect && noChildren) $ e % subEffects .= Just def
updateEffect e (ESetCount n) = e % effectCount .= n
updateEffect e (SetCountInt i) = e % effectCountInt .= i
updateEffect e EToggle1 = e % effectToggle %= not
updateEffect e EToggle2 = e % effectToggle2 %= not
updateEffect e (SubEffectAction action) = updateEffect (e % wrapLens subEffect) action
updateEffect e (SubEffectsAction action) = nelistHelper (e % wrapLens subEffects) updateEffect action
updateEffect e (ESearchTypeAction action) = updateSearchType (e % effectSearchType) action
updateEffect e (EConditionAction action) = updateCondition (e % effectCondition) action

updateCondition :: SubUpdate ConditionAction ConditionModel parent
updateCondition c (SetCondition id) = do
  c % currentCondition .= id
  noChild <- M.gets $ isNothing . (^. c % subCondition)
  when (id == YouMay && noChild) $ c % subCondition .= Just def
  noChildren <- M.gets $ isNothing . (^. c % subConditions)
  when (id == Choose) $ c % subConditions .= Just def
updateCondition c (CSetCount n) = c % conditionCount .= n
updateCondition c CToggle1 = c % conditionToggle %= not
updateCondition c CToggle2 = c % conditionToggle2 %= not
updateCondition c (CSearchTypeAction action) = updateSearchType (c % conditionSearchType) action
updateCondition c (SubConditionAction action) = updateCondition (c % wrapLens subCondition) action
updateCondition c (SubConditionsAction action) = nelistHelper (c % wrapLens subConditions) updateCondition action

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
listHelper l _ NewItem = l %= (++ [def])
listHelper l _ (Delete i) = l %= replace i Nothing
listHelper l u (ItemAction i a) = u (focus l i) a

osetHelper ::
  (Default m, Ord m) =>
  Lens DeckModel (OSet m) ->
  SubUpdate a m parent ->
  ListAction a ->
  Effect parent
osetHelper l u NewItem = l %= (|> def)
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
nelistHelper l u NewItem = l %= flip appendList [def]
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

focus l i =
  let get m = (m ^. l) !! i; set x = l %~ replace i (Just x)
   in lens get (flip set)

cardName :: Lens CardModel M.MisoString
cardName =
  let l m = if m ^. editingSpell then spellStats % spellName else monsterStats % monsterName
      get m = m ^. l m
      set m n = (l m .~ n) m
   in lens get set
