{-# OPTIONS_GHC -Wno-orphans #-}

module ShowCard () where

import Data.Foldable (Foldable (toList))
import Types (Card (..), CardStats (..), Monster (..), Spell (..))
import Utils (collapse, showFold)

instance Show Spell where
  show (Spell n t cs es) = concat [show n, " ", show t, if null cs then ": " else scs, ses]
    where
      scs = " " ++ showFold ", " (toList cs) ++ ": "
      ses = showFold ", " es

instance Show Monster where
  show (Monster n ss rs p t) =
    concat $
      show n
        : ( if null rs
              then []
              else
                [ "\n",
                  showFold ", " $ toList rs,
                  ":"
                ]
          )
        ++ map (\(s, c) -> "\n\t" ++ (if c > 1 then show c ++ "x " else "") ++ show s) (collapse ss)
        ++ [ "\n\tPower ",
             show p,
             if t then "\t[Tapped]" else ""
           ]

instance Show CardStats where
  show (SpellStats s) = show s
  show (MonsterStats s) = show s

instance Show Card where
  show (Card _ fs cs _) =
    concat $
      show cs
        : if null fs
          then []
          else
            [ "\n\t(",
              showFold ", " $ toList fs,
              ")"
            ]
