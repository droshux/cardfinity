module ExampleCards (sampleDeck, sampleDeck2, sampleDeck3) where

import Atoms
import Data.List.NonEmpty (NonEmpty (..))
import Data.Set.Ordered
import Types

sampleDeck :: [Card]
sampleDeck =
  concatMap
    (uncurry replicate)
    [ (10, imp),
      (9, demonWarrior),
      (5, grandsciomancy),
      (5, mysteriousSacrifice),
      (5, darkEngine),
      -- (5, darkPortal),
      (5, arcaneburst),
      (3, freshStart)
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

freshStart :: Card
freshStart =
  Card
    { _cardStats =
        SpellStats $
          Spell
            { _spellTrigger = OnPlay,
              _spellName = "Fresh Start",
              _effects = [Ex $ RequirementEffect $ Ex $ Destroy Discard $ FindCards 100 ForCard Hand, Ex $ Draw 3],
              _castingConditions = reqs $ Destroy Discard $ FindCards 3 ForCard Hand
            },
      _cardID = 0,
      _cardFamilies = empty
    }

sciomancy :: Card
sciomancy =
  Card
    { _cardStats =
        SpellStats $
          Spell
            { _spellTrigger = OnPlay,
              _spellName = "Sciomancy",
              _effects = [Ex $ Peek 1],
              _castingConditions = reqs $ PopGraveyard 1
            },
      _cardID = 0,
      _cardFamilies = fromList ["Occult", "Necromancy", "Divination"]
    }

grandsciomancy :: Card
grandsciomancy =
  Card
    { _cardStats =
        SpellStats $
          Spell
            { _spellTrigger = OnPlay,
              _spellName = "Grand Sciomancy",
              _effects =
                [ Ex $ Peek 1,
                  Ex $ YouMay $ Ex $ Choose (Ex (RequirementEffect $ Ex $ PopGraveyard 1) :| [Ex $ RequirementEffect $ Ex (Destroy Discard (FindCards 1 ForCard Deck) :: DestroyCards)])
                ],
              _castingConditions = reqs $ PopGraveyard 1
            },
      _cardID = 0,
      _cardFamilies = fromList ["Occult", "Necromancy", "Divination"]
    }

mysteriousSacrifice :: Card
mysteriousSacrifice =
  Card
    { _cardStats =
        SpellStats $
          Spell
            { _spellTrigger = OnPlay,
              _spellName = "Mysterious Sacrifice",
              _effects = [Ex $ DestroyTheirs Discard 2 Field],
              _castingConditions = reqs (IfCards $ FindCards 5 ForMonster Graveyard) ~> Destroy Banish (FindCards 1 ForMonster Field)
            },
      _cardID = 0,
      _cardFamilies = empty
    }

darkEngine :: Card
darkEngine =
  Card
    { _cardStats =
        MonsterStats $
          Monster
            { _summoningConditions = empty,
              _monsterSpells =
                [ Spell
                    { _spellTrigger = OnTap,
                      _spellName = "Dark Cycle",
                      _effects = [Ex $ Draw 1],
                      _castingConditions = reqs $ Destroy Discard $ FindCards 1 ForMonster Hand
                    }
                ],
              _monsterName = "Dark Engine",
              _isTapped = False,
              _combatPower = 0
            },
      _cardID = 0,
      _cardFamilies = fromList ["Dark Machine"]
    }

portalSummoning :: OSet (Ex Requirement)
portalSummoning =
  reqs (Destroy Banish $ FindCards 2 ForCard Field)
    ~> IfCards (FindCards 1 ForMonster Graveyard)
    ~> Destroy Discard (FindCards 1 ForMonster Deck)

darkPortal :: Card
darkPortal =
  Card
    { _cardStats =
        MonsterStats $
          Monster
            { _summoningConditions = portalSummoning,
              _monsterSpells =
                [ Spell
                    { _spellTrigger = OnTap,
                      _spellName = "Dark Summoning",
                      _effects = [Ex $ Draw 1],
                      _castingConditions = empty
                    },
                  Spell
                    { _spellTrigger = OnDiscard,
                      _spellName = "Dark Explosion",
                      _effects = [Ex $ DestroyTheirs Banish 1 Field],
                      _castingConditions = reqs $ Destroy Discard $ FindCards 3 ForMonster Deck
                    },
                  Spell {_spellTrigger = OnPlay, _spellName = "Dark Aura", _effects = [Ex $ DestroyTheirs Banish 1 Deck], _castingConditions = empty}
                ],
              _monsterName = "Dark Portal",
              _isTapped = False,
              _combatPower = 0
            },
      _cardID = 0,
      _cardFamilies = fromList ["Dark Machine"]
    }

imp :: Card
imp =
  Card
    { _cardStats =
        MonsterStats $
          Monster
            { _summoningConditions = empty,
              _monsterSpells =
                [ Spell
                    { _spellTrigger = OnDiscard,
                      _spellName = "Chaos Burst",
                      _effects = [Ex $ DestroyTheirs Discard 1 Deck],
                      _castingConditions = reqs $ Destroy Discard $ FindCards 1 (ForName "Arcane Burst") Hand
                    }
                ],
              _monsterName = "Imp",
              _isTapped = False,
              _combatPower = 0
            },
      _cardID = 0,
      _cardFamilies = fromList ["Demon"]
    }

demonWarrior :: Card
demonWarrior =
  Card
    { _cardStats =
        MonsterStats $
          Monster
            { _summoningConditions = reqs $ Destroy Discard $ FindCards 1 ForCard Field,
              _monsterSpells = [Spell {_spellTrigger = OnTap, _spellName = "Flame Spear", _effects = [Ex Attack], _castingConditions = empty}],
              _monsterName = "Demon Warrior",
              _isTapped = False,
              _combatPower = 7
            },
      _cardID = 0,
      _cardFamilies = fromList ["Demon"]
    }

arcaneburst :: Card
arcaneburst =
  Card
    { _cardStats =
        SpellStats $
          Spell
            { _spellTrigger = OnDiscard,
              _spellName = "Arcane Burst",
              _effects = [Ex $ DestroyTheirs Discard 2 Deck],
              _castingConditions = reqs $ Destroy Discard $ FindCards 1 ForSpell Hand
            },
      _cardID = 0,
      _cardFamilies = empty
    }

cherubim :: Card
cherubim =
  Card
    { _cardStats =
        MonsterStats $
          Monster
            { _summoningConditions = empty,
              _monsterSpells =
                [ Spell
                    { _spellTrigger = OnPlay,
                      _spellName = "Call to the Archangels",
                      _effects = [Ex DrawGY],
                      _castingConditions = reqs (Destroy Banish $ FindCards 1 ForCard Hand) ~> Destroy Discard (FindCards 1 (ForFamily "Archangel") Deck)
                    }
                ],
              _monsterName = "Cherubim",
              _isTapped = False,
              _combatPower = 0
            },
      _cardID = 0,
      _cardFamilies = singleton "Angel"
    }

gabriel :: Card
gabriel =
  Card
    { _cardStats =
        MonsterStats $
          Monster
            { _summoningConditions = reqs $ Destroy Discard $ FindCards 1 (ForFamily "Angel") Field,
              _monsterSpells =
                [ Spell
                    { _spellTrigger = OnTap,
                      _spellName = "Messenger of God",
                      _effects = [Ex DrawGY],
                      _castingConditions = reqs $ Destroy Discard $ FindCards 1 ForSpell Hand
                    }
                ],
              _monsterName = "Gabriel",
              _isTapped = False,
              _combatPower = 8
            },
      _cardID = 0,
      _cardFamilies = fromList ["Angel", "Archangel"]
    }

michael :: Card
michael =
  Card
    { _cardStats =
        MonsterStats $
          Monster
            { _summoningConditions = reqs $ Destroy Discard $ FindCards 1 (ForFamily "Angel") Field,
              _monsterSpells =
                [ Spell
                    { _spellTrigger = OnTap,
                      _spellName = "Healer of God",
                      _effects = [Ex $ Heal 1],
                      _castingConditions = empty
                    }
                ],
              _monsterName = "Michael",
              _isTapped = False,
              _combatPower = 6
            },
      _cardID = 0,
      _cardFamilies = fromList ["Angel", "Archangel"]
    }

sacredSacrifice :: Card
sacredSacrifice =
  Card
    { _cardStats =
        SpellStats $
          Spell
            { _spellTrigger = OnPlay,
              _spellName = "Sacred Sacrifice",
              _effects = [Ex $ Heal 3],
              _castingConditions = reqs $ Destroy Discard $ FindCards 1 (ForFamily "Angel") Field
            },
      _cardID = 0,
      _cardFamilies = empty
    }

lazarus :: Card
lazarus =
  Card
    { _cardStats =
        SpellStats $
          Spell
            { _spellTrigger = OnPlay,
              _spellName = "Staff of Lazarus",
              _effects = [Ex PlayGY],
              _castingConditions = reqs $ Destroy Banish $ FindCards 1 ForCard Hand
            },
      _cardID = 0,
      _cardFamilies = empty
    }

prophecy :: Card
prophecy =
  Card
    { _cardStats =
        SpellStats $
          Spell
            { _spellTrigger = OnDraw,
              _spellName = "Prophecy",
              _effects = [Ex $ Peek 1],
              _castingConditions = reqs $ IfCards $ FindCards 1 (ForFamily "Archangel") Field
            },
      _cardID = 0,
      _cardFamilies = empty
    }

divineRetribution :: Card
divineRetribution =
  Card
    { _cardStats =
        SpellStats $
          Spell
            { _spellTrigger = OnDiscard,
              _spellName = "Divine Retribution",
              _effects = [Ex $ DestroyTheirs Discard 2 Deck],
              _castingConditions = reqs $ Destroy Discard $ FindCards 1 (ForFamily "Angel") Deck
            },
      _cardID = 0,
      _cardFamilies = empty
    }

raphael :: Card
raphael =
  Card
    { _cardStats =
        MonsterStats $
          Monster
            { _summoningConditions = reqs (Destroy Discard $ FindCards 1 (ForFamily "Angel") Field),
              _monsterSpells = [Spell {_spellTrigger = OnTap, _spellName = "Divine Wrath", _effects = [Ex Attack], _castingConditions = empty}],
              _monsterName = "Raphael",
              _isTapped = False,
              _combatPower = 9
            },
      _cardID = 0,
      _cardFamilies = fromList ["Angel", "Archangel"]
    }

hermitCrab :: Card
hermitCrab =
  Card
    { _cardStats =
        MonsterStats $
          Monster
            { _summoningConditions = reqs $ Destroy Discard $ FindCards 2 ForCard Hand,
              _monsterSpells =
                [ Spell
                    { _spellTrigger = OnTap,
                      _spellName = "Scavenge Shell",
                      _effects = [Ex Attach],
                      _castingConditions = empty
                    }
                ],
              _monsterName = "Hermit Crab",
              _isTapped = False,
              _combatPower = 5
            },
      _cardID = 0,
      _cardFamilies = singleton "Crab"
    }

staboCrabo :: Card
staboCrabo =
  Card
    { _cardStats =
        SpellStats $
          Spell
            { _spellTrigger = OnTap,
              _spellName = "Stabbo Crabbo",
              _effects = [Ex AttackDirectly],
              _castingConditions = reqs $ Destroy Discard (FindCards 3 ForCard Deck)
            },
      _cardID = 0,
      _cardFamilies = singleton "Crabcessory"
    }

fancyHat :: Card
fancyHat =
  Card
    { _cardStats =
        SpellStats $
          Spell
            { _spellTrigger = OnTap,
              _spellName = "Fancy Hat",
              _effects = [Ex DrawGY],
              _castingConditions = reqs (PopGraveyard 1) ~> Destroy Discard (FindCards 1 (ForFamily "Crabcessory") Deck)
            },
      _cardID = 0,
      _cardFamilies = singleton "Crabcessory"
    }

shrimpPistol :: Card
shrimpPistol =
  Card
    { _cardStats =
        SpellStats $
          Spell
            { _spellTrigger = OnTap,
              _spellName = "Shrimp Pistol",
              _effects = [Ex $ DestroyTheirs Discard 1 Field],
              _castingConditions = reqs $ Destroy Discard $ FindCards 1 ForCard Hand
            },
      _cardID = 0,
      _cardFamilies = singleton "Crabcessory"
    }

crabcaine :: Card
crabcaine =
  Card
    { _cardStats =
        SpellStats $
          Spell
            { _spellTrigger = OnTap,
              _spellName = "Crabcaine",
              _effects = [Ex $ SearchFor $ ForFamily "Crabcessory"],
              _castingConditions = reqs (IfCards $ FindCards 20 ForCard Graveyard) ~> Destroy Discard (FindCards 1 ForSpell Hand)
            },
      _cardID = 0,
      _cardFamilies = singleton "Crabcessory"
    }

crabCycle :: Card
crabCycle =
  Card
    { _cardStats =
        SpellStats $
          Spell
            { _spellTrigger = OnDiscard,
              _spellName = "The Crab Cycle",
              _effects = [Ex DrawGY],
              _castingConditions = reqs $ Destroy Banish $ FindCards 1 ForCard Deck
            },
      _cardID = 0,
      _cardFamilies = singleton "Crabcessory"
    }
