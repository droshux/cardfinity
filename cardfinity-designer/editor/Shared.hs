{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

module Shared (
searchTypeEditor ,
) where

import qualified Miso as M
import qualified Miso.Html as H
import qualified Atoms as A
import Miso.Lens (Lens,lens, (.=), (<%=), (%=), (^.))
import Miso.Lens.TH (makeLenses)
import Atoms (SearchType(..))
import Miso (text)

data SearchTypeModel = STModel {
    _searchType :: SearchType,
    _currentText :: M.MisoString
}

$(makeLenses ''SearchTypeModel)

data SearchTypeAction = SetType M.MisoString | SetText M.MisoString

searchTypeEditor :: M.Component parentModel SearchTypeModel SearchTypeAction
searchTypeEditor  = M.component def update view
    where
        def = STModel ForCard ""
        update (SetType s) | s == "Card" = searchType .= ForCard
            | s == "Monster" = searchType .= ForMonster
            | s == "Spell" = searchType .= ForSpell
            | s == "Name" = do
                t <- currentText <%= id
                searchType .= ForName (show t)
            | s == "Family" = do
                t <- currentText <%= id
                searchType .= ForFamily (show t)
        update (SetText t) = do
            currentText .= t
            searchType %= \case
                ForName _ -> ForName (show t)
                ForFamily _ -> ForFamily (show t)
                other -> other
        view m =
          H.span_
            [] $ H.select_
                [H.onChange SetType ]
                [
                H.option_ [] [text "Card"],
                H.option_ [] [text "Monster"],
                H.option_ [] [text "Spell"],
                H.option_ [] [text "Name"],
                H.option_ [] [text "Family"]
                ] : [H.input_ [H.onChange SetText]| hasTxt (m ^. searchType)]
        hasTxt (ForName _) = True
        hasTxt (ForFamily _) = True
        hasTxt  _ = False
