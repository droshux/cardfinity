{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Theme.Selector (selector, defaultTheme, currentTheme, Theme, themeClass) where

import Miso qualified as M
import Miso.Html qualified as H
import Miso.Html.Property qualified as P
import Miso.Lens (Lens, lens, (.=), (^.))
import Miso.Lens.TH (makeLenses)
import Miso.String qualified as M

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

styleSheets :: [M.CSS]
styleSheets = map (flip M.Href False . toFile) $ filter (/= Custom) $ enumFrom $ toEnum 0
  where
    toFile :: Theme -> M.MisoString
    toFile t = "assets/themes/" <> themeClass t <> ".css"

newtype Model = Model
  { _currentTheme :: Theme
  }
  deriving (Eq)

$(makeLenses ''Model)

newtype Action = SetTheme Theme

selector :: M.Component parent props Model Action
selector =
  (M.component initialState update view)
    { M.styles = styleSheets
    }
  where
    initialState = (Model {_currentTheme = defaultTheme})
    update (SetTheme t) = currentTheme .= t

-- TODO: Make a nicer selector?
view :: props -> Model -> M.View Model Action
view _ m =
  let options = map opt $ enumFrom $ toEnum 0
   in H.select_
        [H.onChange (SetTheme . toEnum . M.fromMisoString)]
        options
  where
    opt :: Theme -> M.View Model Action
    opt t =
      H.option_
        [ P.value_ (M.toMisoString $ fromEnum t)
        ]
        [ M.text $ case t of
            BasicLight -> "Clean (Light)"
            BasicDark -> "Clean (Dark)"
            Custom -> "Custom theme"
        ]
