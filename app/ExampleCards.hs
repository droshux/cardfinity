module ExampleCards where

import Atoms
import Data.List.NonEmpty (NonEmpty (..))
import Data.Set.Ordered
import Types
import Utils (SearchType (..))

sampleDeck :: [Card]
sampleDeck =
  concatMap
    (uncurry replicate)
    [ (8, imp),
      (6, hellishPustule),
      (5, demonWarrior),
      (5, stimulant),
      --      (5, demonBrute),
      --      (5, demonArcher),
      (3, emergencyRetreat),
      (5, sciomancy),
      (3, grandSciomancy),
      (8, arcaneburst),
      (7, darkEngine),
      (5, darkPortal)
    ]

chaosFlame :: Card
chaosFlame =
  Card
    { _cardStats =
        MonsterStats $
          Monster
            { _summoningConditions = singleton $ destroyCards Discard $ FindCardsHand 1 $ ForName "Chaos Flame",
              _monsterSpells = [Spell {_spellTrigger = OnDiscard, _spellName = "Engulf", _effects = [dealDamage 2 False], _castingConditions = singleton discardDeck}],
              _monsterName = "Chaos Flame",
              _isTapped = False,
              _combatPower = 0
            },
      _cardID = 0,
      _cardFamilies = singleton "Gamebreaking?"
    }

sciomancy :: Card
sciomancy =
  Card
    { _cardStats =
        SpellStats $
          Spell
            { _spellTrigger = OnPlay,
              _spellName = "Sciomancy",
              _effects = [peek 1],
              _castingConditions = singleton $ popGraveyard 1
            },
      _cardID = 0,
      _cardFamilies = empty
    }

mindSpike :: Card
mindSpike =
  Card
    { _cardStats =
        SpellStats $
          Spell
            { _spellTrigger = OnPlay,
              _spellName = "Mind Spike",
              _effects = [scry 1, youMay $ choose (discardTheirDeck :| [dealDamage 1 True])],
              _castingConditions = singleton (takeDamage 2 True)
            },
      _cardID = 0,
      _cardFamilies = empty
    }

stimulant :: Card
stimulant =
  Card
    { _cardStats =
        SpellStats $
          Spell
            { _spellTrigger = OnPlay,
              _spellName = "Stimulants",
              _effects = [alterPower 3 False],
              _castingConditions = empty
            },
      _cardID = 0,
      _cardFamilies = empty
    }

druidMentor :: Card
druidMentor =
  Card
    { _cardStats =
        MonsterStats $
          Monster
            { _summoningConditions = singleton (destroyCards Banish (FindCardsField 1 ForCard)),
              _monsterSpells =
                [ Spell
                    { _spellTrigger = OnTap,
                      _spellName = "Circle of Life",
                      _effects = [heal 1, drawEffect 1],
                      _castingConditions = singleton (destroyCards Discard (FindCardsHand 1 ForCard))
                    }
                ],
              _monsterName = "Druid Mentor",
              _isTapped = False,
              _combatPower = 0
            },
      _cardID = 0,
      _cardFamilies = empty
    }

grandSciomancy :: Card
grandSciomancy =
  Card
    { _cardStats =
        SpellStats $
          Spell
            { _spellTrigger = OnPlay,
              _spellName = "Grand Sciomancy",
              _effects = [peek 1, youMay $ choose (asEffect (takeDamage 1 False) :| [asEffect discardDeck])],
              _castingConditions = singleton $ popGraveyard 1
            },
      _cardID = 0,
      _cardFamilies = empty
    }

emergencyRetreat :: Card
emergencyRetreat =
  Card
    { _cardStats =
        SpellStats $
          Spell
            { _spellTrigger = OnPlay,
              _spellName = "Emergency Retreat",
              _effects = [heal 1, drawEffect 1],
              _castingConditions = singleton (popGraveyard 3) |> destroyCards Discard (FindCardsField 1 ForMonster)
            },
      _cardID = 0,
      _cardFamilies = empty
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
                      _effects = [dealDamage 1 False],
                      _castingConditions = singleton $ destroyCards Discard $ FindCardsHand 1 (ForName "Arcane Burst")
                    }
                ],
              _monsterName = "Imp",
              _isTapped = False,
              _combatPower = 0
            },
      _cardID = 0,
      _cardFamilies = singleton "Demon"
    }

demonWarrior :: Card
demonWarrior =
  Card
    { _cardStats =
        MonsterStats $
          Monster
            { _summoningConditions = singleton $ destroyCards Discard $ FindCardsField 1 ForMonster,
              _monsterSpells = [Spell {_spellTrigger = OnTap, _spellName = "Flame Spear", _effects = [attack False], _castingConditions = empty}],
              _monsterName = "Demon Warrior",
              _isTapped = False,
              _combatPower = 5
            },
      _cardID = 0,
      _cardFamilies = fromList ["Demon"]
    }

demonBrute :: Card
demonBrute =
  Card
    { _cardStats =
        MonsterStats $
          Monster
            { _summoningConditions = empty,
              _monsterSpells =
                [ Spell
                    { _spellTrigger = OnTap,
                      _spellName = "Desperate Rage",
                      _effects = [attack False],
                      _castingConditions = singleton $ popGraveyard 8
                    }
                ],
              _monsterName = "Demon Berzerker",
              _isTapped = False,
              _combatPower = 8
            },
      _cardID = 0,
      _cardFamilies = singleton "Demon"
    }

demonArcher :: Card
demonArcher =
  Card
    { _cardStats =
        MonsterStats $
          Monster
            { _summoningConditions = singleton (destroyCards Discard $ FindCardsField 1 ForMonster) |> takeDamage 1 False,
              _monsterSpells =
                [ Spell
                    { _spellTrigger = OnTap,
                      _spellName = "Hellfire Rain",
                      _effects = [attack True],
                      _castingConditions = singleton $ destroyCards Discard $ FindCardsHand 1 ForCard
                    }
                ],
              _monsterName = "Demon Archer",
              _isTapped = False,
              _combatPower = 5
            },
      _cardID = 0,
      _cardFamilies = singleton "Demon"
    }

hellishPustule :: Card
hellishPustule =
  Card
    { _cardStats =
        MonsterStats $
          Monster
            { _summoningConditions = empty,
              _monsterSpells =
                [ Spell
                    { _spellTrigger = OnDefeat,
                      _spellName = "Pus Explosion",
                      _effects = [destroyTheirCards Discard (FindCardsField 1 ForMonster)],
                      _castingConditions = singleton $ destroyCards Discard $ FindCardsHand 2 ForCard
                    }
                ],
              _monsterName = "Hellish Pustule",
              _isTapped = False,
              _combatPower = 0
            },
      _cardID = 0,
      _cardFamilies = singleton "Demon"
    }

arcaneburst :: Card
arcaneburst =
  Card
    { _cardStats =
        SpellStats $
          Spell
            { _spellTrigger = OnDiscard,
              _spellName = "Arcane Burst",
              _effects = [dealDamage 2 False],
              _castingConditions = singleton $ destroyCards Discard $ FindCardsHand 1 ForSpell
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
            { _summoningConditions = singleton $ popGraveyard 2,
              _monsterSpells =
                [ Spell
                    { _spellTrigger = OnTap,
                      _spellName = "Dark Cycle",
                      _effects = [drawEffect 1],
                      _castingConditions = singleton $ destroyCards Discard $ FindCardsHand 1 ForCard
                    }
                ],
              _monsterName = "Dark Engine",
              _isTapped = False,
              _combatPower = 0
            },
      _cardID = 0,
      _cardFamilies = fromList ["Dark Machine"]
    }

darkPortal :: Card
darkPortal =
  Card
    { _cardStats =
        MonsterStats $
          Monster
            { _summoningConditions = singleton (destroyCards Discard $ FindCardsField 1 (ForFamily "Dark Machine")) |> destroyCards Discard (FindCardsHand 1 (ForFamily "Demon")),
              _monsterSpells =
                [ Spell
                    { _spellTrigger = OnTap,
                      _spellName = "Dark Summoning",
                      _effects = [drawEffect 1],
                      _castingConditions = empty
                    },
                  Spell
                    { _spellTrigger = OnDefeat,
                      _spellName = "Dimensional Rift",
                      _effects = [destroyTheirCards Discard (FindCardsField 1 ForMonster)],
                      _castingConditions = singleton $ takeDamage 2 False
                    }
                ],
              _monsterName = "Dark Portal",
              _isTapped = False,
              _combatPower = 0
            },
      _cardID = 0,
      _cardFamilies = singleton "Dark Machine"
    }

hermitCrab :: Card
hermitCrab =
  Card
    { _cardStats =
        MonsterStats $
          Monster
            { _summoningConditions = singleton (destroyCards Discard (FindCardsHand 1 ForCard)),
              _monsterSpells =
                [ Spell
                    { _spellTrigger = OnTap,
                      _spellName = "Upgrades people upgrades!",
                      _effects = [attach ForCard],
                      _castingConditions = empty
                    }
                ],
              _monsterName = "Hermit Crab",
              _isTapped = False,
              _combatPower = 5
            },
      _cardID = 0,
      _cardFamilies = fromList ["Crustacean"]
    }
