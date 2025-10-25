{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

module Shared
  ( searchTypeEditor,
    findCardsEditor,
    searchType,
    findCards,
  )
where

import qualified Miso as M
import qualified Miso.Html as H
import qualified Miso.Html.Property as P
import qualified Atoms as A
import Miso.Lens
    ( Lens, lens, (.=), (<%=), (%=), (^.), (+=), (-=), compose )
import Miso.Lens.TH (makeLenses)
import Atoms (SearchType(..))
import Miso (text)
import GHC.Natural (Natural)
import Miso.Types ((+>), Component (bindings), (<-->))
import Text.Read (readMaybe)
import Data.Maybe (fromMaybe)
import Control.Monad (unless)
import qualified Miso.CSS as P

data SearchTypeModel = STModel {
    _searchType :: SearchType,
    _currentText :: M.MisoString
} deriving (Eq)

$(makeLenses ''SearchTypeModel)

data SearchTypeAction = SetType M.MisoString | SetText M.MisoString

searchTypeEditor :: M.Component parentModel SearchTypeModel SearchTypeAction
searchTypeEditor  = M.component def update view
    where
        def = STModel ForCard ""
        update (SetType "card") = searchType .= ForCard
        update (SetType "monster") = searchType .= ForMonster
        update (SetType "spell") = searchType .= ForSpell
        update (SetType "family") = do
            t <- M.gets (M.fromMisoString  . _currentText)
            searchType .= ForFamily t
        update (SetType "name") = do
            t <- M.gets (M.fromMisoString  . _currentText)
            searchType .= ForName t
        update (SetText t) = do
            currentText .= t
            searchType %= \case
                ForName _ -> ForName (M.fromMisoString t)
                ForFamily _ -> ForFamily (M.fromMisoString t)
                st -> st
        view m =
          H.span_
            []
            [ H.select_ [H.onInput SetType, P.value_ (getValue $ m^.searchType )]
                [ H.option_ [P.value_ "card"] [text "Card"],
                  H.option_ [P.value_ "monster"] [text "Monster"],
                  H.option_ [P.value_ "spell"] [text "Spell"],
                  H.option_ [P.value_ "name"] [text "Name"],
                  H.option_ [P.value_ "family"] [text "Family"]
                ],
                H.input_ [
                    H.onInput SetText,
                    (P.value_ . M.toMisoString  . getText . _searchType ) m,
                    P.style_ [("display", "none") | hideInput $ m^.searchType ]
                ]
            ]
        hideInput (ForFamily _)  = False
        hideInput (ForName _)= False
        hideInput  _ = True
        getValue ForCard = "card"
        getValue ForMonster = "monster"
        getValue ForSpell = "spell"
        getValue (ForFamily _) = "family"
        getValue (ForName _) = "name"
        getText (ForName t) = t
        getText (ForFamily t) = t
        getText _ = ""

data FindCardsModel = FCModel {
    _searchTypeFC :: SearchType,
    _count :: Natural,
    _isHand :: Bool
} deriving (Eq)

findCards :: Lens FindCardsModel A.FindCards
findCards = let
    get (FCModel st c True) = A.FindCardsHand c st
    get (FCModel st c False) = A.FindCardsField c st
    set _ = \case
        A.FindCardsHand c st -> FCModel st c True
        A.FindCardsField c st -> FCModel st c False

    in lens get set

$(makeLenses  ''FindCardsModel)

data FindCardsAction = Inc | Dec | Toggle

findCardsEditor :: M.Component parentModel FindCardsModel FindCardsAction
findCardsEditor = M.component def update view
    where
        def = FCModel {_searchTypeFC=ForCard, _count = 0, _isHand = True}
        update Inc = count += 1
        update Dec = do
            c <- M.gets (^. count)
            unless (c == 0) (count -= 1)
        update Toggle = do
            findCards %= \case
                A.FindCardsHand n st -> A.FindCardsField n st
                A.FindCardsField n st -> A.FindCardsHand n st
        view m = H.span_ [] [
            H.button_ [H.onClick Toggle] [text "Toggle"],
            H.button_ [H.onClick Inc] [text "+"],
            H.button_ [H.onClick Dec] [text "-"],
            H.span_ []  +> (searchTypeEditor {bindings=[searchTypeFC <--> searchType ]})
            ]
