{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Export (ExportProps (ExportProps), export) where

import CardParser qualified as CF
import Data.Base64.Types (extractBase64)
import Data.Char (isAlphaNum)
import Data.Text (pack)
import Data.Text.Encoding.Base64.URL (encodeBase64)
import Miso qualified as M
import Miso.Html qualified as H
import Miso.Html.Property qualified as P
import Miso.Lens qualified as M ((^.))
import Miso.Lens.TH (makeLenses)
import Types qualified as CF

newtype ExportProps = ExportProps
  { _deck :: CF.DeckInfo
  }
  deriving (Eq)

$(makeLenses ''ExportProps)

export :: M.Component ctx ExportProps () ()
export = M.component () M.noop view

view :: ctx -> ExportProps -> () -> M.View ctx () ()
view _ props _ =
  H.a_
    [ P.href_ $ exportURL $ props M.^. deck,
      P.download_ $ M.toMisoString $ formatFileName $ CF.deckName $ props M.^. deck
    ]
    [M.text "Download Deck"]

urlPrefix :: M.MisoString
urlPrefix = "data:text/plain;base64,"

exportURL :: CF.DeckInfo -> M.MisoString
exportURL =
  (urlPrefix <>)
    . M.toMisoString
    . extractBase64
    . encodeBase64
    . pack
    . CF.unparseDeck False -- Potentially allow concise export?

formatFileName :: String -> String
formatFileName dn = filter isAlphaNum dn ++ ".txt"
