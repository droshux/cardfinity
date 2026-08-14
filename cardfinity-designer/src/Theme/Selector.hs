{-# LANGUAGE OverloadedStrings #-}

module Theme.Selector (selector) where

import Context (Context, theme)
import Miso qualified as M
import Miso.Html qualified as H
import Miso.Html.Property qualified as P
import Miso.Lens qualified as M
import Theme.Types

styleSheets :: [M.CSS]
styleSheets = map (flip M.Href False . toFile) $ filter (/= Custom) $ enumFrom $ toEnum 0
  where
    toFile :: Theme -> M.MisoString
    toFile t = "assets/themes/" <> themeClass t <> ".css"

newtype Action = SetTheme Theme

selector :: M.Component Context props () Action
selector =
  (M.component () update view)
    { M.styles = styleSheets,
      M.useContext = True
    }
  where
    update (SetTheme t) = M.modifyContext (theme M..~ t)

-- TODO: Make a nicer selector?
view :: Context -> props -> () -> M.View Context Action
view _ _ _ =
  let options = map opt $ enumFrom $ toEnum 0
   in H.select_
        [H.onChange (SetTheme . toEnum . M.fromMisoString)]
        options
  where
    opt :: Theme -> M.View ctx Action
    opt t =
      H.option_ [P.value_ (M.toMisoString $ fromEnum t)] [M.text (themeName t)]
