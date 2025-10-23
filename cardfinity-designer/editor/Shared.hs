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
import Miso.Types ((+>), Component (bindings), (<--))
import Text.Read (readMaybe)
import Data.Maybe (fromMaybe)
import Control.Monad (unless)
import qualified Miso.CSS as P

data SearchTypeModel = STModel {
    _currentType :: M.MisoString,
    _currentText :: M.MisoString
} deriving (Eq)

searchType :: Lens SearchTypeModel SearchType
searchType = let 
    get (STModel "name" t) = ForName $ M.fromMisoString  t
    get (STModel "family" t) = ForFamily $ M.fromMisoString t
    get (STModel "monster" _) = ForMonster
    get (STModel "spell" _) = ForSpell
    get (STModel _ _) = ForCard
    set m ForCard = m {_currentType = "card"}
    set m ForMonster = m {_currentType  = "monster"}
    set m ForSpell = m {_currentType  = "spell"}
    set m (ForName s) = m {_currentType ="name", _currentText = M.toMisoString s}
    set m (ForFamily s) = m {_currentType ="family", _currentText = M.toMisoString s}
    in lens get set

$(makeLenses ''SearchTypeModel)

data SearchTypeAction = SetType M.MisoString | SetText M.MisoString

searchTypeEditor :: M.Component parentModel SearchTypeModel SearchTypeAction
searchTypeEditor  = M.component def update view
    where
        def = STModel "card" ""
        update (SetType t) = currentType .= t
        update (SetText t) = currentText .= t
        view m =
          H.span_
            []
            [ H.select_ [H.onInput SetType]
                [ H.option_ [P.value_ "card"] [text "Card"],
                  H.option_ [P.value_ "monster"] [text "Monster"],
                  H.option_ [P.value_ "spell"] [text "Spell"],
                  H.option_ [P.value_ "name"] [text "Name"],
                  H.option_ [P.value_ "family"] [text "Family"]
                ],
                H.input_ [
                    H.onInput SetText,
                    P.style_ [("display", "none") | not $ hasTxt $ m^.currentType ] 
                ],
                H.span_ [] [text $ m^.currentText]
            ]
        hasTxt "name" = True
        hasTxt "family" = True
        hasTxt  _ = False

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
            H.span_ [] [text $ M.toMisoString $ show $ m ^.searchTypeFC],
            H.button_ [H.onClick Toggle] [text "Toggle"],
            H.button_ [H.onClick Inc] [text "+"],
            H.button_ [H.onClick Dec] [text "-"],
            H.span_ []  +> (searchTypeEditor {bindings=[searchTypeFC <-- searchType ]})
            ]
