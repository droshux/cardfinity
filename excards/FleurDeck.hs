{-# OPTIONS_GHC -Wno-missing-signatures #-}

module FleurDeck where

import Atoms
import Data.List.NonEmpty (NonEmpty (..))
import Data.Set.Ordered
import Types
import Utils (SearchType (..))

makeSacrifice n = singleton $ destroyCards Discard $ FindCardsField n ForMonster

discHand n = singleton $ destroyCards Discard $ FindCardsHand n ForCard

fleurdeck :: [Card]
fleurdeck =
  concatMap
    (uncurry replicate)
    [ (13, rat),
      (5, pidgeon),
      (3, cat),
      (2, wizenedHermit),
      (4, emergencySacrifice),
      (1, celerityPidgeon),
      (2, stormboundHerald),
      (2, wtow),
      (4, ratConvenor),
      (4, owl),
      (5, vampire),
      (1, longwell),
      (1, electBlood),
      (2, visionTwoPaths),
      (2, retributiveCurse),
      (2, axisMundi),
      (2, protectiveWards),
      (2, earthenBull),
      (1, occultDiviner),
      (1, tribBrinEn),
      (1, ratKing)
    ]

rat :: Card
rat =
  Card
    { _cardStats =
        MonsterStats $
          Monster
            { _summoningConditions = empty,
              _monsterSpells = [],
              _monsterName = "Rat",
              _isTapped = False,
              _combatPower = 0
            },
      _cardID = 0,
      _cardFamilies = fromList ["Rodent", "Sacrifice"]
    }

pidgeon :: Card
pidgeon =
  Card
    { _cardStats =
        MonsterStats $
          Monster
            { _summoningConditions = empty,
              _monsterSpells = [],
              _monsterName = "Pidgeon",
              _isTapped = False,
              _combatPower = 0
            },
      _cardID = 0,
      _cardFamilies = fromList ["Pidgeon", "Sacrifice"]
    }

cat :: Card
cat =
  Card
    { _cardStats =
        MonsterStats $
          Monster
            { _summoningConditions = singleton $ takeDamage 1 False,
              _monsterSpells =
                [ Spell
                    { _spellTrigger = OnDiscard,
                      _spellName = "Lucky Sacrifice",
                      _effects = [alterPower 1 False],
                      _castingConditions = empty
                    }
                ],
              _monsterName = "Cat",
              _isTapped = False,
              _combatPower = 0
            },
      _cardID = 0,
      _cardFamilies = singleton "Sacrifice"
    }

wizenedHermit :: Card
wizenedHermit =
  Card
    { _cardStats =
        MonsterStats $
          Monster
            { _summoningConditions = makeSacrifice 2,
              _monsterSpells =
                [ Spell
                    { _spellTrigger = OnTap,
                      _spellName = "Gift of The Hag",
                      _effects = replicate 2 $ alterPower 3 False,
                      _castingConditions = makeSacrifice 1
                    },
                  Spell
                    { _spellTrigger = OnDraw,
                      _spellName = "Learned One",
                      _effects = [choose (attach ForSpell :| [drawEffect 1, peek 3])],
                      _castingConditions = empty
                    }
                ],
              _monsterName = "Wizened Hermit",
              _isTapped = False,
              _combatPower = 0
            },
      _cardID = 0,
      _cardFamilies = fromList ["Vampire", "Blood Sorcery"]
    }

emergencySacrifice :: Card
emergencySacrifice =
  Card
    { _cardStats =
        SpellStats $
          Spell
            { _spellTrigger = OnPlay,
              _spellName = "Emergency Sacrifice",
              _effects = [search $ SearchFor $ ForFamily "Sacrifice"],
              _castingConditions = makeSacrifice 2
            },
      _cardID = 0,
      _cardFamilies = singleton "Sacrifice"
    }

celerityPidgeon :: Card
celerityPidgeon =
  Card
    { _cardStats =
        SpellStats $
          Spell
            { _spellTrigger = OnPlay,
              _spellName = "Celerity Pidgeon",
              _effects = [dealDamage 3 False],
              _castingConditions = singleton $ destroyCards Discard $ FindCardsField 1 $ ForName "Pidgeon"
            },
      _cardID = 0,
      _cardFamilies = empty
    }

stormboundHerald :: Card
stormboundHerald =
  Card
    { _cardStats =
        MonsterStats $
          Monster
            { _summoningConditions = makeSacrifice 1,
              _monsterSpells = [Spell {_spellTrigger = OnDiscard, _spellName = "Potent Sacrifice", _effects = [drawEffect 2], _castingConditions = singleton $ popGraveyard 5}],
              _monsterName = "Stormbound Herald",
              _isTapped = False,
              _combatPower = 3
            },
      _cardID = 0,
      _cardFamilies = fromList ["Vampire", "Blood Sorcery", "Sacrifice"]
    }

wtow :: Card
wtow =
  Card
    { _cardStats =
        SpellStats $
          Spell
            { _spellTrigger = OnPlay,
              _spellName = "Wrestle the Old Woman",
              _effects = [alterPower 12 False],
              _castingConditions = singleton $ destroyCards Discard $ FindCardsField 2 ForMonster
            },
      _cardID = 0,
      _cardFamilies = singleton "Blood Sorcery"
    }

ratConvenor :: Card
ratConvenor =
  Card
    { _cardStats =
        MonsterStats $
          Monster
            { _summoningConditions = discHand 4,
              _monsterSpells =
                [ Spell
                    { _spellTrigger = OnTap,
                      _spellName = "Rat Graft",
                      _effects = [heal 5],
                      _castingConditions = singleton (destroyCards Discard $ FindCardsField 1 $ ForFamily "Rodent") |> destroyCards Discard (FindCardsHand 1 ForCard)
                    },
                  Spell
                    { _spellTrigger = OnTap,
                      _spellName = "Conjure the swarm!",
                      _effects = [search $ SearchFor $ ForFamily "Rodent"],
                      _castingConditions = discHand 2
                    }
                ],
              _monsterName = "Rat Convenor",
              _isTapped = False,
              _combatPower = 3
            },
      _cardID = 0,
      _cardFamilies = fromList ["Vampire", "Scientist"]
    }

tribBrinEn :: Card
tribBrinEn =
  Card
    { _cardStats =
        SpellStats $
          Spell
            { _spellTrigger = OnPlay,
              _spellName = "Tribulation brings enlightenment",
              _effects = [drawEffect 3],
              _castingConditions = discHand 2
            },
      _cardID = 0,
      _cardFamilies = singleton "Blood Sorcery"
    }

owl :: Card
owl =
  Card
    { _cardStats =
        MonsterStats $
          Monster
            { _summoningConditions = discHand 1,
              _monsterSpells =
                [ Spell
                    { _spellTrigger = OnPlay,
                      _spellName = "Swoop",
                      _effects = [attack False],
                      _castingConditions = empty
                    }
                ],
              _monsterName = "Owl",
              _isTapped = False,
              _combatPower = 2
            },
      _cardID = 0,
      _cardFamilies = fromList ["Bird", "Sacrifice"]
    }

studyForgottenTexts :: Card
studyForgottenTexts =
  Card
    { _cardStats =
        SpellStats $
          Spell
            { _spellTrigger = OnPlay,
              _spellName = "Study the forgotten texts",
              _effects = [drawEffect 2],
              _castingConditions = singleton $ takeDamage 2 False
            },
      _cardID = 0,
      _cardFamilies = empty
    }

vampire :: Card
vampire =
  Card
    { _cardStats =
        MonsterStats $
          Monster
            { _summoningConditions = makeSacrifice 1,
              _monsterSpells =
                [ Spell
                    { _spellTrigger = OnTap,
                      _spellName = "Fangs",
                      _effects = [attack False],
                      _castingConditions = empty
                    }
                ],
              _monsterName = "Vampire",
              _isTapped = False,
              _combatPower = 2
            },
      _cardID = 0,
      _cardFamilies = singleton "Vampire"
    }

ratKing :: Card
ratKing =
  Card
    { _cardStats =
        MonsterStats $
          Monster
            { _summoningConditions = singleton $ takeDamage 2 False,
              _monsterSpells =
                [ Spell
                    { _spellTrigger = OnDiscard,
                      _spellName = "Vile Sacrifice",
                      _effects = [heal 1],
                      _castingConditions = empty
                    }
                ],
              _monsterName = "Rat King",
              _isTapped = False,
              _combatPower = 0
            },
      _cardID = 0,
      _cardFamilies = fromList ["Rodent", "Sacrifice"]
    }

longwell :: Card
longwell =
  Card
    { _cardStats =
        MonsterStats $
          Monster
            { _summoningConditions = makeSacrifice 1,
              _monsterSpells =
                [ Spell
                    { _spellTrigger = OnTap,
                      _spellName = "Frenzy",
                      _effects = [alterPower 1 True, attack False],
                      _castingConditions = singleton $ takeDamage 1 True
                    }
                ],
              _monsterName = "Wasteful Hierophant",
              _isTapped = False,
              _combatPower = 2
            },
      _cardID = 0,
      _cardFamilies = fromList ["Vampire", "Blood Sorcery"]
    }

electBlood :: Card
electBlood =
  Card
    { _cardStats =
        SpellStats $
          Spell
            { _spellTrigger = OnPlay,
              _spellName = "Election by Blood",
              _effects = [search $ SearchFor $ ForName "Wasteful Hierophant", playCardEffect $ ForName "Wasteful Hierophant", alterPower 2 False],
              _castingConditions = singleton $ takeDamage 6 True
            },
      _cardID = 0,
      _cardFamilies = singleton "Blood Sorcery"
    }

visionTwoPaths :: Card
visionTwoPaths =
  Card
    { _cardStats =
        SpellStats $
          Spell
            { _spellTrigger = OnDraw,
              _spellName = "Vision of two paths",
              _effects = [peek 2, youMay $ asEffect $ takeDamage 1 False],
              _castingConditions = singleton $ takeDamage 1 True
            },
      _cardID = 0,
      _cardFamilies = fromList ["Divination", "Blood Sorcery"]
    }

retributiveCurse :: Card
retributiveCurse =
  Card
    { _cardStats =
        SpellStats $
          Spell
            { _spellTrigger = OnDiscard,
              _spellName = "Retributive Curse",
              _effects = [dealDamage 2 False],
              _castingConditions = singleton $ popGraveyard 5
            },
      _cardID = 0,
      _cardFamilies = fromList ["Blood Sorcery", "Curse"]
    }

leeroy :: Card
leeroy =
  Card
    { _cardStats =
        MonsterStats $
          Monster
            { _summoningConditions = makeSacrifice 1,
              _monsterSpells =
                [ Spell
                    { _spellTrigger = OnTap,
                      _spellName = "Accursed Collection",
                      _effects = [attach $ ForFamily "Curse", alterPower 1 True],
                      _castingConditions = singleton $ takeDamage 1 True
                    }
                ],
              _monsterName = "Curse Specialist",
              _isTapped = False,
              _combatPower = 3
            },
      _cardID = 0,
      _cardFamilies = fromList ["Vampire", "Blood Sorcery", "Curse"]
    }

occultDiviner :: Card
occultDiviner =
  Card
    { _cardStats =
        MonsterStats $
          Monster
            { _summoningConditions = makeSacrifice 1 |> takeDamage 1 False,
              _monsterSpells =
                [ Spell
                    { _spellTrigger = OnTap,
                      _spellName = "Gaze into the future",
                      _effects = [peek 2],
                      _castingConditions = empty
                    },
                  Spell
                    { _spellTrigger = OnDiscard,
                      _spellName = "Final Divination",
                      _effects = [peek 5],
                      _castingConditions = singleton (takeDamage 2 True) |> destroyCards Banish (FindCardsField 1 ForMonster)
                    }
                ],
              _monsterName = "Occult Diviner",
              _isTapped = False,
              _combatPower = 0
            },
      _cardID = 0,
      _cardFamilies = empty
    }

protectiveWards :: Card
protectiveWards =
  Card
    { _cardStats =
        MonsterStats $
          Monster
            { _summoningConditions = discHand 1,
              _monsterSpells = [],
              _monsterName = "Protective Wards",
              _isTapped = False,
              _combatPower = 10
            },
      _cardID = 0,
      _cardFamilies = singleton "Blood Sorcery"
    }

earthenBull :: Card
earthenBull =
  Card
    { _cardStats =
        MonsterStats $
          Monster
            { _summoningConditions = makeSacrifice 2,
              _monsterSpells =
                [ Spell
                    { _spellTrigger = OnTap,
                      _spellName = "Gore",
                      _effects = [attack False],
                      _castingConditions = empty
                    },
                  Spell {_spellTrigger = OnDiscard, _spellName = "Resilient", _effects = [heal 1, drawEffect 1], _castingConditions = singleton $ takeDamage 3 True}
                ],
              _monsterName = "Earthen Bull",
              _isTapped = False,
              _combatPower = 5
            },
      _cardID = 0,
      _cardFamilies = singleton "Sacrifice"
    }

axisMundi :: Card
axisMundi =
  Card
    { _cardStats =
        MonsterStats $
          Monster
            { _summoningConditions = makeSacrifice 1,
              _monsterSpells =
                [ Spell
                    { _spellTrigger = OnTap,
                      _spellName = "Wellspring of potential",
                      _effects = [drawEffect 1],
                      _castingConditions = empty
                    }
                ],
              _monsterName = "Axis Mundi",
              _isTapped = False,
              _combatPower = 4
            },
      _cardID = 0,
      _cardFamilies = empty
    }
