module Editor.Mapping (deckFromModel, cardFromModel) where

import Atoms qualified as CF
import Data.Foldable (Foldable (toList))
import Data.Function ((&))
import Data.List.NonEmpty qualified as NE
import Data.Set.Ordered qualified as OS
import Editor.Types
import GHC.Natural (Natural)
import Miso (fromMisoString)
import Miso.Lens (Lens, (^.))
import Types qualified as CF

deckFromModel :: DeckModel -> [CF.Card]
deckFromModel d = d ^. deck >>= zipWith cardFromModel [0 ..] . uncurry replicate

cardFromModel :: Natural -> CardModel -> CF.Card
cardFromModel id =
  CF.Card
    <$> const id
    <*> osetMap fromMisoString . (^. families)
    <*> ifelse (^. editingSpell) (CF.SpellStats . spellFromModel . (^. spellStats)) (CF.MonsterStats . monsterFromModel . (^. monsterStats))
    <*> ifelse (== "") (const Nothing) Just . fromMisoString . (^. imageUrl)

spellFromModel :: SpellModel -> CF.Spell
spellFromModel =
  CF.Spell
    <$> fromMisoString . (^. spellName)
    <*> (^. spellTrigger)
    <*> osetMap conditionFromModel . (^. castingConditions)
    <*> map effectFromModel . (^. spellEffects)

monsterFromModel :: MonsterModel -> CF.Monster
monsterFromModel =
  CF.Monster
    <$> fromMisoString . (^. monsterName)
    <*> map spellFromModel . (^. monsterSpells)
    <*> osetMap conditionFromModel . (^. summoningConditions)
    <*> (^. combatPower)
    <*> (^. entersTapped)

conditionFromModel :: ConditionModel -> CF.Condition
conditionFromModel m =
  m & case m ^. currentCondition of
    DiscardSelf -> const CF.DiscardSelf
    Destroy -> destroyFromModel CF.Destroy conditionSearchType conditionCount conditionToggle conditionToggle2
    TakeDamage -> CF.TakeDamage <$> (^. conditionCount) <*> (^. conditionToggle)
    HealOpponent -> CF.HealOpponent . (^. conditionCount)
    Pop -> CF.Pop . (^. conditionCount)
    YouMay -> CF.YouMay . maybe CF.DiscardSelf conditionFromModel . (^. subCondition)
    Choose -> CF.Choose . maybe (CF.DiscardSelf NE.:| []) (fmap conditionFromModel) . (^. subConditions)

effectFromModel :: EffectModel -> CF.Effect
effectFromModel m =
  m & case m ^. currentEffect of
    DiscardEnemy -> const CF.DiscardEnemy
    DestroyEnemy -> destroyFromModel CF.DestroyEnemy effectSearchType effectCount effectToggle effectToggle2
    DealDamage -> CF.DealDamage <$> (^. effectCount) <*> (^. effectToggle)
    Heal -> CF.Heal . (^. effectCount)
    DECKOUT -> const CF.DECKOUT
    Draw -> CF.Draw . (^. effectCount)
    Peek -> CF.Peek . (^. effectCount)
    Scry -> CF.Scry . (^. effectCount)
    Optional -> CF.Optional . maybe CF.DiscardEnemy effectFromModel . (^. subEffect)
    ChooseEffect -> CF.ChooseEffect . maybe (CF.DiscardEnemy NE.:| []) (fmap effectFromModel) . (^. subEffects)
    Attack -> CF.Attack . (^. effectToggle)
    Play -> CF.Play . stFromModel . (^. effectSearchType)
    Search ->
      let st = stFromModel . (^. effectSearchType)
       in CF.Search . ifelse (^. effectToggle) (CF.SearchFor . st) (CF.DrillFor . st)
    Attach -> CF.Attach . stFromModel . (^. effectSearchType)
    Buff -> CF.Buff <$> (^. effectCountInt) <*> (^. effectToggle)
    AsEffect -> CF.AsEffect . conditionFromModel . (^. effectCondition)

destroyFromModel f st n b1 b2 =
  f
    <$> ifelse (^. b1) (const CF.Banish) (const CF.Discard)
    <*> ifelse (^. b2) (CF.FindCardsField <$> (^. n) <*> stFromModel . (^. st)) (CF.FindCardsHand <$> (^. n) <*> stFromModel . (^. st))

stFromModel :: SearchTypeModel -> CF.SearchType
stFromModel m =
  case m ^. searchTypeID of
    ForCard -> CF.ForCard
    ForMonster -> CF.ForMonster
    ForSpell -> CF.ForSpell
    ForName -> CF.ForName $ fromMisoString $ m ^. searchTypeText
    ForFamily -> CF.ForFamily $ fromMisoString $ m ^. searchTypeText

osetMap :: (Ord b) => (a -> b) -> OS.OSet a -> OS.OSet b
osetMap f = OS.fromList . map f . toList

ifelse b f g x = if b x then f x else g x
