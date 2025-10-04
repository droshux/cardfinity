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
import Miso.Lens (Lens,lens, (.=), (<%=), (%=), (^.), (+=), (-=))
import Miso.Lens.TH (makeLenses)
import Atoms (SearchType(..))
import Miso (text)
import GHC.Natural (Natural)
import Miso.Types ((+>), Component (bindings), (<--))
import Text.Read (readMaybe)
import Data.Maybe (fromMaybe)
import Miso.Lens (compose)

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
            compose txt searchType .= Just (show t)
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
        txt = let 
            get = \case
                ForName t -> Just t
                ForFamily t -> Just t
                other -> Nothing
            set st Nothing = st
            set (ForName _) (Just t) = ForName t
            set (ForFamily _) (Just t) = ForFamily t
            set st _ = st
            in lens get set

data FindCardsModel = FCModel {
    _searchTypeFC :: SearchType,
    _findCards :: A.FindCards
} deriving (Eq)

$(makeLenses  ''FindCardsModel)

data FindCardsAction = SetCount Natural | Inc | Dec | Toggle

findCardsEditor :: M.Component parentModel FindCardsModel FindCardsAction
findCardsEditor = M.component def update view
    where
        def = FCModel {_searchTypeFC=ForCard, _findCards=A.FindCardsHand 0 ForCard}
        count = lens A.getCount  $ \f n -> case f of
            A.FindCardsHand _ st -> A.FindCardsHand n st
            A.FindCardsField _ st -> A.FindCardsField n st
        update Inc = compose count findCards += 1 
        update Dec = compose count findCards -= 1
        update (SetCount n) = compose count findCards .= n 
        update Toggle = do
            findCards %= \case
                A.FindCardsHand n st -> A.FindCardsField n st
                A.FindCardsField n st -> A.FindCardsHand n st
        view _ = H.div_ [] [
            H.button_ [H.onClick Toggle] [text "Toggle"],
            H.button_ [H.onClick Inc] [text "+"],
            H.input_ [P.type_ "number",H.onChange (SetCount . parseNat)],
            H.button_ [H.onClick Dec] [text "-"],
            H.span_ []  +> (searchTypeEditor {bindings=[searchTypeFC <-- searchType]})
            ]
        parseNat :: M.MisoString -> Natural
        parseNat  = fromMaybe 0 . readMaybe . show
