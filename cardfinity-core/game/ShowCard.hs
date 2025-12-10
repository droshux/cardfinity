{-# OPTIONS_GHC -Wno-orphans #-}

module ShowCard () where

import Data.Foldable (Foldable (toList))
import GHC.Stats (GCDetails (gcdetails_block_fragmentation_bytes))
import Optics.Operators ((^.))
import Types
import Utils (collapse, showFold)

instance Show Spell where
  show spell = concat [show (spell ^. spellName), " ", show (spell ^. spellTrigger), if null (spell ^. castingConditions) then ": " else scs, ses]
    where
      scs = " " ++ showFold ", " (toList $ spell ^. castingConditions) ++ ": "
      ses = showFold ", " $ spell ^. effects

instance Show Monster where
  show monster =
    concat $
      show (monster ^. monsterName)
        : ( if null (monster ^. summoningConditions)
              then []
              else
                [ "\n",
                  showFold ", " $ toList $ monster ^. summoningConditions,
                  ":"
                ]
          )
        ++ map (\(s, c) -> "\n\t" ++ (if c > 1 then show c ++ "x " else "") ++ show s) (collapse $ monster ^. monsterSpells)
        ++ [ "\n\tPower ",
             show $ monster ^. combatPower,
             if monster ^. isTapped then "\t[Tapped]" else ""
           ]

instance Show CardStats where
  show = cardStatsElim show show

instance Show Card where
  show card =
    concat $
      show (card ^. cardStats)
        : if null (card ^. cardFamilies)
          then []
          else
            [ "\n\t(",
              showFold ", " $ toList $ card ^. cardFamilies,
              ")"
            ]
