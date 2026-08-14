module Editor.Mapping (deckFromModel, cardFromModel) where

import Atoms qualified as CF
import Data.Function ((&))
import Data.List.NonEmpty qualified as NE
import Data.Maybe (fromMaybe)
import Data.Set.Ordered qualified as OS
import Editor.Types
import GHC.Natural (Natural)
import Miso (fromMisoString)
import Miso.Lens (Lens, (^.))
import Types qualified as CF

deckFromModel :: DeckModel -> [CF.Card]
deckFromModel d = d ^. deck >>= zipWith cardFromModel [0 ..] . uncurry replicate

cardFromModel :: Natural -> CardModel -> CF.Card
cardFromModel cardId =
  CF.Card
    <$> const cardId
    <*> OS.fromList . map fromMisoString . (^. families)
    <*> ifelse (^. editingSpell) (CF.SpellStats . spellFromModel . (^. spellStats)) (CF.MonsterStats . monsterFromModel . (^. monsterStats))
    <*> ifelse (== "") (const Nothing) Just . fromMisoString . (^. imageUrl)

spellFromModel :: SpellModel -> CF.Spell
spellFromModel =
  CF.Spell
    <$> fromMisoString . (^. spellName)
    <*> (toTrigger . (^. spellTrigger))
    <*> OS.fromList . map conditionFromModel . (^. castingConditions)
    <*> map effectFromModel . (^. spellEffects)

monsterFromModel :: MonsterModel -> CF.Monster
monsterFromModel =
  CF.Monster
    <$> fromMisoString . (^. monsterName)
    <*> map spellFromModel . (^. monsterSpells)
    <*> OS.fromList . map conditionFromModel . (^. summoningConditions)
    <*> (^. combatPower)
    <*> const False
    <*> (^. entersTapped)

conditionFromModel :: ConditionModel -> CF.Condition
conditionFromModel m =
  m & case m ^. currentCondition of
    DiscardSelf -> const CF.DiscardSelf
    Destroy -> destroyFromModel CF.Destroy conditionSearchType conditionCount conditionToggle conditionToggle2 conditionToggle3
    TakeDamage -> CF.TakeDamage <$> (^. conditionCount) <*> (^. conditionToggle)
    HealOpponent -> CF.HealOpponent . (^. conditionCount)
    Pop -> CF.Pop . (^. conditionCount)
    YouMay -> CF.YouMay . conditionFromModel . applyOptionalCond
    Choose -> CF.Choose . fromMaybe (CF.DiscardSelf NE.:| []) . toNE . map conditionFromModel . (^. subConditions)

effectFromModel :: EffectModel -> CF.Effect
effectFromModel m =
  m & case m ^. currentEffect of
    DiscardEnemy -> const CF.DiscardEnemy
    DestroyEnemy -> destroyFromModel CF.DestroyEnemy effectSearchType effectCount effectToggle effectToggle2 effectToggle3
    DealDamage -> CF.DealDamage <$> (^. effectCount) <*> (^. effectToggle)
    Heal -> CF.Heal . (^. effectCount)
    DECKOUT -> const CF.DECKOUT
    Draw -> CF.Draw . (^. effectCount)
    Peek -> CF.Peek . (^. effectCount)
    Scry -> CF.Scry . (^. effectCount)
    Optional -> CF.Optional . effectFromModel . applyOptionalEff
    ChooseEffect -> CF.ChooseEffect . fromMaybe (CF.DiscardEnemy NE.:| []) . toNE . map effectFromModel . (^. subEffects)
    Attack -> CF.Attack . (^. effectToggle)
    Play -> CF.Play . stFromModel . (^. effectSearchType)
    Search ->
      let st = stFromModel . (^. effectSearchType)
       in CF.Search . ifelse (^. effectToggle) (CF.SearchFor . st) (CF.DrillFor . st)
    Attach -> CF.Attach . stFromModel . (^. effectSearchType)
    Buff -> CF.Buff <$> (^. effectCountInt) <*> (^. effectToggle)
    AsEffect -> CF.AsEffect . conditionFromModel . (^. effectCondition)

destroyFromModel :: (CF.DestroyType -> CF.FindCards -> b) -> Lens t SearchTypeModel -> Lens t Natural -> Lens t Bool -> Lens t Bool -> Lens t Bool -> t -> b
destroyFromModel f st n b1 b2 b3 =
  f
    <$> ifelse (^. b1) (const CF.Banish) (const CF.Discard)
    <*> ifelse (^. b2) (CF.FindCardsField <$> (^. n) <*> (^. b3) <*> stFromModel . (^. st)) (CF.FindCardsHand <$> (^. n) <*> stFromModel . (^. st))

stFromModel :: SearchTypeModel -> CF.SearchType
stFromModel m =
  case m ^. searchTypeID of
    ForCard -> CF.ForCard
    ForMonster -> CF.ForMonster
    ForSpell -> CF.ForSpell
    ForName -> CF.ForName $ fromMisoString $ m ^. searchTypeText
    ForFamily -> CF.ForFamily $ fromMisoString $ m ^. searchTypeText

toNE :: [a] -> Maybe (NE.NonEmpty a)
toNE [] = Nothing
toNE (x : xs) = Just $ x NE.:| xs

ifelse :: (t1 -> Bool) -> (t1 -> t2) -> (t1 -> t2) -> t1 -> t2
ifelse b f g x = if b x then f x else g x
