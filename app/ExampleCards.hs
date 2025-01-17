module ExampleCards where

import Atoms
import Data.Set.Ordered
import Types

sciomancy :: Card
sciomancy =
  Card
    { cardStats =
        SpellStats $
          Spell
            { spellTrigger = OnPlay,
              spellName = "Sciomancy",
              effects = [Ex $ Peek 1],
              castingConditions = reqs $ PopGraveyard 1
            },
      cardID = 0,
      cardFamilies = fromList ["Occult", "Necromancy", "Divination"]
    }

sampleDeck :: [Card]
sampleDeck = concatMap (uncurry replicate) [(10, imp), (10, demonWarrior), (5, sciomancy), (5, mysteriousSacrifice), (5, darkEngine), (5, darkPortal)]

mysteriousSacrifice :: Card
mysteriousSacrifice =
  Card
    { cardStats =
        SpellStats $
          Spell
            { spellTrigger = OnPlay,
              spellName = "Mysterious Sacrifice",
              effects = [Ex $ DestroyTheirs Discard 2 Field],
              castingConditions = reqs (IfCards $ FindCards 5 ForMonster Graveyard) ~> Destroy Banish (FindCards 1 ForMonster Field)
            },
      cardID = 0,
      cardFamilies = empty
    }

darkEngine :: Card
darkEngine =
  Card
    { cardStats =
        MonsterStats $
          Monster
            { summoningConditions = empty,
              monsterSpells =
                [ Spell
                    { spellTrigger = OnTap,
                      spellName = "Dark Cycle",
                      effects = [Ex $ Draw 1],
                      castingConditions = reqs $ Destroy Discard $ FindCards 1 ForMonster Hand
                    }
                ],
              monsterName = "Dark Engine",
              isTapped = False,
              combatPower = 0
            },
      cardID = 0,
      cardFamilies = empty
    }

portalSummoning :: OSet (Ex Requirement)
portalSummoning =
  singleton (Ex $ Destroy Banish $ FindCards 2 ForCard Field)
    |> Ex (IfCards $ FindCards 1 ForMonster Graveyard)
    |> Ex (Destroy Discard $ FindCards 1 ForMonster Deck)

darkPortal :: Card
darkPortal =
  Card
    { cardStats =
        MonsterStats $
          Monster
            { summoningConditions = portalSummoning,
              monsterSpells =
                [ Spell
                    { spellTrigger = OnTap,
                      spellName = "Dark Summoning",
                      effects = [Ex $ Draw 1],
                      castingConditions = empty
                    },
                  Spell
                    { spellTrigger = OnDiscard,
                      spellName = "Dark Explosion",
                      effects = [Ex $ DestroyTheirs Banish 1 Field],
                      castingConditions = reqs $ Destroy Discard $ FindCards 3 ForMonster Deck
                    },
                  Spell {spellTrigger = OnPlay, spellName = "Dark Aura", effects = [Ex $ DestroyTheirs Banish 1 Deck], castingConditions = empty}
                ],
              monsterName = "Dark Portal",
              isTapped = False,
              combatPower = 0
            },
      cardID = 0,
      cardFamilies = empty
    }

imp :: Card
imp =
  Card
    { cardStats =
        MonsterStats $
          Monster
            { summoningConditions = empty,
              monsterSpells =
                [ Spell
                    { spellTrigger = OnDiscard,
                      spellName = "Chaos Burst",
                      effects = [Ex $ DestroyTheirs Discard 1 Deck],
                      castingConditions = reqs $ Destroy Discard $ FindCards 1 (ForName "Arcane Burst") Hand
                    }
                ],
              monsterName = "Imp",
              isTapped = False,
              combatPower = 0
            },
      cardID = 0,
      cardFamilies = empty
    }

demonWarrior :: Card
demonWarrior =
  Card
    { cardStats =
        MonsterStats $
          Monster
            { summoningConditions = reqs $ Destroy Discard $ FindCards 1 ForCard Field,
              monsterSpells = [Spell {spellTrigger = OnTap, spellName = "Flame Spear", effects = [Ex Attack], castingConditions = empty}],
              monsterName = "Demon Warrior",
              isTapped = False,
              combatPower = 5
            },
      cardID = 0,
      cardFamilies = empty
    }

arcaneburst :: Spell
arcaneburst =
  Spell
    { spellTrigger = OnDiscard,
      spellName = "Arcane Burst",
      effects = [Ex $ DestroyTheirs Discard 2 Deck],
      castingConditions = reqs $ Destroy Discard $ FindCards 1 ForSpell Hand
    }
