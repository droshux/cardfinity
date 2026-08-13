{-# LANGUAGE OverloadedStrings #-}

module Theme.Types (Theme (..), defaultTheme, themeClass) where

import Miso qualified as M

data Theme
  = BasicLight
  | BasicDark
  | Custom
  deriving (Eq, Enum)

defaultTheme :: Theme
defaultTheme = BasicLight

-- CSS class names
themeClass :: Theme -> M.MisoString
themeClass BasicLight = "basic-light"
themeClass BasicDark = "basic-dark"
themeClass Custom = "custom"
