module Main where

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

darkPortal :: Card
darkPortal =
  Card
    { cardStats =
        MonsterStats $
          Monster
            { summoningConditions = fromList [],
              monsterSpells =
                [ Spell
                    { spellTrigger = OnPlay,
                      spellName = "Summoning",
                      effects = [Ex $ Draw 1],
                      castingConditions = fromList []
                    }
                ],
              monsterName = "Dark Portal",
              isTapped = True,
              combatPower = 0
            },
      cardID = 0,
      cardFamilies = fromList []
    }

main :: IO ()
main = putStrLn "Hello, Haskell!"
