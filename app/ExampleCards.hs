module ExampleCards where

import Atoms
import Data.Set.Ordered
import Types

sampleDeck :: [Card]
sampleDeck =
  concatMap
    (uncurry replicate)
    [ (10, imp),
      (9, demonWarrior),
      (5, sciomancy),
      (5, mysteriousSacrifice),
      (5, darkEngine),
      (5, darkPortal),
      (5, arcaneburst)
    ]

sampleDeck2 :: [Card]
sampleDeck2 =
  concatMap
    (uncurry replicate)
    [ (5, raphael),
      (3, gabriel),
      (5, michael),
      (10, cherubim),
      (3, sacredSacrifice),
      (5, lazarus),
      (5, prophecy),
      (5, divineRetribution)
    ]

sampleDeck3 :: [Card]
sampleDeck3 = concatMap (uncurry replicate) $ (20, hermitCrab) : map (5,) [staboCrabo, fancyHat, shrimpPistol, crabcaine, crabCycle]

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
      cardFamilies = fromList ["Dark Machine"]
    }

portalSummoning :: OSet (Ex Requirement)
portalSummoning =
  reqs (Destroy Banish $ FindCards 2 ForCard Field)
    ~> IfCards (FindCards 1 ForMonster Graveyard)
    ~> Destroy Discard (FindCards 1 ForMonster Deck)

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
      cardFamilies = fromList ["Dark Machine"]
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
      cardFamilies = fromList ["Demon"]
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
              combatPower = 7
            },
      cardID = 0,
      cardFamilies = fromList ["Demon"]
    }

arcaneburst :: Card
arcaneburst =
  Card
    { cardStats =
        SpellStats $
          Spell
            { spellTrigger = OnDiscard,
              spellName = "Arcane Burst",
              effects = [Ex $ DestroyTheirs Discard 2 Deck],
              castingConditions = reqs $ Destroy Discard $ FindCards 1 ForSpell Hand
            },
      cardID = 0,
      cardFamilies = empty
    }

cherubim :: Card
cherubim =
  Card
    { cardStats =
        MonsterStats $
          Monster
            { summoningConditions = empty,
              monsterSpells =
                [ Spell
                    { spellTrigger = OnPlay,
                      spellName = "Call to the Archangels",
                      effects = [Ex DrawGY],
                      castingConditions = reqs (Destroy Banish $ FindCards 1 ForCard Hand) ~> Destroy Discard (FindCards 1 (ForFamily "Archangel") Deck)
                    }
                ],
              monsterName = "Cherubim",
              isTapped = False,
              combatPower = 0
            },
      cardID = 0,
      cardFamilies = singleton "Angel"
    }

gabriel :: Card
gabriel =
  Card
    { cardStats =
        MonsterStats $
          Monster
            { summoningConditions = reqs $ Destroy Discard $ FindCards 1 (ForFamily "Angel") Field,
              monsterSpells =
                [ Spell
                    { spellTrigger = OnTap,
                      spellName = "Messenger of God",
                      effects = [Ex DrawGY],
                      castingConditions = reqs $ Destroy Discard $ FindCards 1 ForSpell Hand
                    }
                ],
              monsterName = "Gabriel",
              isTapped = False,
              combatPower = 8
            },
      cardID = 0,
      cardFamilies = fromList ["Angel", "Archangel"]
    }

michael :: Card
michael =
  Card
    { cardStats =
        MonsterStats $
          Monster
            { summoningConditions = reqs $ Destroy Discard $ FindCards 1 (ForFamily "Angel") Field,
              monsterSpells =
                [ Spell
                    { spellTrigger = OnTap,
                      spellName = "Healer of God",
                      effects = [Ex $ Heal 1],
                      castingConditions = empty
                    }
                ],
              monsterName = "Michael",
              isTapped = False,
              combatPower = 6
            },
      cardID = 0,
      cardFamilies = fromList ["Angel", "Archangel"]
    }

sacredSacrifice :: Card
sacredSacrifice =
  Card
    { cardStats =
        SpellStats $
          Spell
            { spellTrigger = OnPlay,
              spellName = "Sacred Sacrifice",
              effects = [Ex $ Heal 3],
              castingConditions = reqs $ Destroy Discard $ FindCards 1 (ForFamily "Angel") Field
            },
      cardID = 0,
      cardFamilies = empty
    }

lazarus :: Card
lazarus =
  Card
    { cardStats =
        SpellStats $
          Spell
            { spellTrigger = OnPlay,
              spellName = "Staff of Lazarus",
              effects = [Ex PlayGY],
              castingConditions = reqs $ Destroy Banish $ FindCards 1 ForCard Hand
            },
      cardID = 0,
      cardFamilies = empty
    }

prophecy :: Card
prophecy =
  Card
    { cardStats =
        SpellStats $
          Spell
            { spellTrigger = OnDraw,
              spellName = "Prophecy",
              effects = [Ex $ Peek 1],
              castingConditions = reqs $ IfCards $ FindCards 1 (ForFamily "Archangel") Field
            },
      cardID = 0,
      cardFamilies = empty
    }

divineRetribution :: Card
divineRetribution =
  Card
    { cardStats =
        SpellStats $
          Spell
            { spellTrigger = OnDiscard,
              spellName = "Divine Retribution",
              effects = [Ex $ DestroyTheirs Discard 2 Deck],
              castingConditions = reqs $ Destroy Discard $ FindCards 1 (ForFamily "Angel") Deck
            },
      cardID = 0,
      cardFamilies = empty
    }

raphael :: Card
raphael =
  Card
    { cardStats =
        MonsterStats $
          Monster
            { summoningConditions = reqs (Destroy Discard $ FindCards 1 (ForFamily "Angel") Field),
              monsterSpells = [Spell {spellTrigger = OnTap, spellName = "Divine Wrath", effects = [Ex Attack], castingConditions = empty}],
              monsterName = "Raphael",
              isTapped = False,
              combatPower = 9
            },
      cardID = 0,
      cardFamilies = fromList ["Angel", "Archangel"]
    }

hermitCrab :: Card
hermitCrab =
  Card
    { cardStats =
        MonsterStats $
          Monster
            { summoningConditions = reqs $ Destroy Discard $ FindCards 2 ForCard Hand,
              monsterSpells =
                [ Spell
                    { spellTrigger = OnTap,
                      spellName = "Scavenge Shell",
                      effects = [Ex Attach],
                      castingConditions = empty
                    }
                ],
              monsterName = "Hermit Crab",
              isTapped = False,
              combatPower = 5
            },
      cardID = 0,
      cardFamilies = singleton "Crab"
    }

staboCrabo :: Card
staboCrabo =
  Card
    { cardStats =
        SpellStats $
          Spell
            { spellTrigger = OnTap,
              spellName = "Stabbo Crabbo",
              effects = [Ex AttackDirectly],
              castingConditions = reqs $ Destroy Discard (FindCards 3 ForCard Deck)
            },
      cardID = 0,
      cardFamilies = singleton "Crabcessory"
    }

fancyHat :: Card
fancyHat =
  Card
    { cardStats =
        SpellStats $
          Spell
            { spellTrigger = OnTap,
              spellName = "Fancy Hat",
              effects = [Ex DrawGY],
              castingConditions = reqs (PopGraveyard 1) ~> Destroy Discard (FindCards 1 (ForFamily "Crabcessory") Deck)
            },
      cardID = 0,
      cardFamilies = singleton "Crabcessory"
    }

shrimpPistol :: Card
shrimpPistol =
  Card
    { cardStats =
        SpellStats $
          Spell
            { spellTrigger = OnTap,
              spellName = "Shrimp Pistol",
              effects = [Ex $ DestroyTheirs Discard 1 Field],
              castingConditions = reqs $ Destroy Discard $ FindCards 1 ForCard Hand
            },
      cardID = 0,
      cardFamilies = singleton "Crabcessory"
    }

crabcaine :: Card
crabcaine =
  Card
    { cardStats =
        SpellStats $
          Spell
            { spellTrigger = OnTap,
              spellName = "Crabcaine",
              effects = [Ex $ SearchFor $ ForFamily "Crabcessory"],
              castingConditions = reqs (IfCards $ FindCards 20 ForCard Graveyard) ~> Destroy Discard (FindCards 1 ForSpell Hand)
            },
      cardID = 0,
      cardFamilies = singleton "Crabcessory"
    }

crabCycle :: Card
crabCycle =
  Card
    { cardStats =
        SpellStats $
          Spell
            { spellTrigger = OnDiscard,
              spellName = "The Crab Cycle",
              effects = [Ex DrawGY],
              castingConditions = reqs $ Destroy Banish $ FindCards 1 ForCard Deck
            },
      cardID = 0,
      cardFamilies = singleton "Crabcessory"
    }
