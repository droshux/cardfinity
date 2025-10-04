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

data SearchTypeModel = STModel {
    _currentType :: M.MisoString,
    _currentText :: M.MisoString
} deriving (Eq)

searchType :: Lens SearchTypeModel SearchType
searchType = let 
    get (STModel st t) | st == "Card" = ForCard
        | st == "Monster" = ForMonster
        | st == "Spell" = ForSpell
        | st == "Name" = ForName $ M.fromMisoString t
        | st == "Family" = ForFamily $ M.fromMisoString t
    set (STModel st t) = \case
        ForCard -> STModel "Card" t
        ForMonster -> STModel "Monster" t
        ForSpell -> STModel "Spell" t
        ForName t' -> STModel "Name" (M.toMisoString t')
        ForFamily t' -> STModel "Family" (M.toMisoString t')
    in lens get set

$(makeLenses ''SearchTypeModel)

data SearchTypeAction = SetType M.MisoString | SetText M.MisoString

searchTypeEditor :: M.Component parentModel SearchTypeModel SearchTypeAction
searchTypeEditor  = M.component def update view
    where
        def = STModel "Card" ""
        update (SetType t) = currentType .= t
        update (SetText t) = currentText .= t
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
                ] : [H.input_ [
                    H.onChange SetText,
                    P.type_ "text",
                    P.value_ (m ^. currentText)
                ]| hasTxt (m ^. currentType)]
        hasTxt "Name" = True
        hasTxt "Family" = True
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

data FindCardsAction = SetCount Natural | Inc | Dec | Toggle

findCardsEditor :: M.Component parentModel FindCardsModel FindCardsAction
findCardsEditor = M.component def update view
    where
        def = FCModel {_searchTypeFC=ForCard, _count = 0, _isHand = True}
        update Inc = count += 1
        update Dec = count -= 1
        update (SetCount n) = count .= n
        update Toggle = do
            findCards %= \case
                A.FindCardsHand n st -> A.FindCardsField n st
                A.FindCardsField n st -> A.FindCardsHand n st
        view m = H.div_ [] [
            H.button_ [H.onClick Toggle] [text "Toggle"],
            H.button_ [H.onClick Inc] [text "+"],
            H.input_ [
                P.type_ "number",
                H.onChange (SetCount . parseNat),
                P.value_ (M.toMisoString $ show (m ^. count))
            ],
            H.button_ [H.onClick Dec] [text "-"],
            H.span_ []  +> (searchTypeEditor {bindings=[searchTypeFC <-- searchType]})
            ]
        parseNat :: M.MisoString -> Natural
        parseNat  = fromMaybe 0 . readMaybe . M.fromMisoString 
