{-# LANGUAGE OverloadedStrings #-}

module Theme.Types (Theme (..), defaultTheme, themeClass, themeName) where

import Miso qualified as M

data Theme
  = BasicLight
  | BasicDark
  | Playtest
  | Custom
  deriving (Eq, Enum, Bounded)

defaultTheme :: Theme
defaultTheme = BasicLight

-- CSS class names
themeClass :: Theme -> M.MisoString
themeClass BasicLight = "basic-light"
themeClass BasicDark = "basic-dark"
themeClass Playtest = "playtest"
themeClass Custom = "custom"

themeName :: Theme -> M.MisoString
themeName BasicLight = "Clean (Light)"
themeName BasicDark = "Clean (Dark)"
themeName Playtest = "Playtest "
themeName Custom = "Custom theme"
