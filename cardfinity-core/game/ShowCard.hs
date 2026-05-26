{-# OPTIONS_GHC -Wno-orphans #-}

module ShowCard (show'Spell) where

import AtomDisplay ()
import Data.Foldable (Foldable (toList))
import Optics.Operators ((^.))
import Types
import Utils (collapse, delimFoldMap, show'Fold)

instance Display Spell where
  unparse c s =
    concat
      [ show (s ^. spellName),
        " ",
        unparse c (s ^. spellTrigger),
        if null (s ^. castingConditions) then "" else " ",
        delimFoldMap (unparse c) ", " $ s ^. castingConditions,
        ": ",
        delimFoldMap (unparse c) ", " $ s ^. effects
      ]
  show' = show'Spell True

show'Spell :: Bool -> Spell -> CardText
show'Spell showName s =
  mconcat
    [ if showName then CardName (s ^. spellName) <> txt " " else mempty,
      txt " ",
      Trigger (s ^. spellTrigger),
      if null (s ^. castingConditions)
        then mempty
        else txt " " <> show'Fold (txt ", ") (s ^. castingConditions),
      txt ": ",
      show'Fold (txt ", ") (s ^. effects)
    ]

instance Show Spell where show = show . show'

instance Display Monster where
  unparse c m =
    concat
      [ show (m ^. monsterName),
        ":",
        if null (m ^. summoningConditions) then "" else '\n' : delimFoldMap id (if c then "," else ", ") (map (unparse c) $ toList $ m ^. summoningConditions),
        if null (m ^. monsterSpells) then "" else '\n' : delimFoldMap id "\n" (map (unparse c) $ toList $ m ^. monsterSpells),
        "\npower",
        if c then " " else ": ",
        show (m ^. combatPower),
        "\n",
        if m ^. isTapped then "tapped" else ""
      ]
  show' m =
    mconcat
      [ CardName (m ^. monsterName),
        if null (m ^. summoningConditions)
          then mempty
          else NewLine <> show'Fold (txt ", ") (m ^. summoningConditions) <> txt ":",
        mconcat $ map (\(c, n) -> NewLine <> Indent <> Copies (show' c) n) $ collapse $ m ^. monsterSpells,
        NewLine <> Indent <> Keyword "Power" <> txt " " <> num (m ^. combatPower),
        if m ^. isTapped then txt " " <> Keyword "Tapped" else mempty
      ]

instance Show Monster where show = show . show'

instance Display CardStats where
  unparse c = cardStatsElim (unparse c) (unparse c)
  show' = cardStatsElim show' show'

instance Show CardStats where show = show . show'

instance Display Card where
  unparse c card =
    unparse c (card ^. cardStats)
      ++ if null (card ^. cardFamilies)
        then ""
        else
          "\n("
            ++ delimFoldMap show ", " (card ^. cardFamilies)
            ++ ")"
    -- TODO: Image URL
  show' c =
    show' (c ^. cardStats)
      <> NewLine
      <> delimFoldMap CardFamily (txt ", ") (c ^. cardFamilies)

instance Show Card where show = show . show'
