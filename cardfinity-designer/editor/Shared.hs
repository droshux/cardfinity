{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

module Shared
  ( searchTypeEditor,
    findCardsEditor,
    searchType,
    findCards,
    damageEditor,
    Bound,
    bind,
    DestroyModel(DestroyM),
    destroyEditor,
    natEditor,
    chooseEditor ,
    optionalEditor ,
    atomEditor,
    noLens
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
import qualified Data.List.NonEmpty as NE (NonEmpty ((:|)), map,cons, head, tail)
import GHC.Natural (Natural)
import Miso.Types ((+>), Component (bindings), (<-->))
import Text.Read (readMaybe)
import Data.Bifunctor (second, first)
import Data.Maybe (fromMaybe)
import Control.Monad (unless, when)
import Data.List ((!?), findIndex)
import qualified Miso.CSS as P

data SearchTypeModel = STModel {
    _searchType' :: SearchType,
    _currentText :: M.MisoString
} deriving (Eq)

$(makeLenses ''SearchTypeModel)

searchType = lens _searchType' $ \m -> \case 
    st@(ForName s) -> STModel st (M.toMisoString  s)
    st@(ForFamily s) -> STModel st (M.toMisoString  s)
    st -> STModel st (_currentText m)
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
            [ H.select_ [H.onInput SetType , P.value_ (getValue $ m^.searchType ) ]
                [ H.option_ [P.value_ "card"] [text "Card"],
                  H.option_ [P.value_ "monster"] [text "Monster"],
                  H.option_ [P.value_ "spell"] [text "Spell"],
                  H.option_ [P.value_ "name"] [text "Name"],
                  H.option_ [P.value_ "family"] [text "Family"]
                ],
                H.input_ [
                    H.onInput SetText,
                    P.value_ (m^.currentText),
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

data GenericActions = Toggle | Inc | Dec

findCardsEditor :: M.Component parentModel FindCardsModel GenericActions
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


damageEditor :: M.Component a (Natural,Bool) (Maybe Natural)
damageEditor = M.component (0,False) update view
    where update Nothing = M.modify (second not)
          update (Just n) = M.modify (first (const n))
          view (n,_) = H.span_ [] [
            H.button_ [H.onClick Nothing] [text "Toggle"],
            H.input_ [
                P.type_ "number",
                P.min_ "0",
                P.value_ (M.toMisoString $ show n),
                H.onChange (Just . read . M.fromMisoString )
            ]
              ]


data DestroyModel = DestroyM {
    _banish :: Bool,
    _findCardsD :: A.FindCards
} deriving (Eq)

$(makeLenses ''DestroyModel)

destroyEditor :: M.Component a DestroyModel GenericActions
destroyEditor = M.component def update view
    where
        def =  DestroyM False $ A.FindCardsHand 0 A.ForCard
        update Toggle = banish %= not
        update _ = return ()
        view _ = H.span_ [] [
            H.button_ [H.onClick Toggle] [text "Banish"],
            H.span_ [] +> (findCardsEditor {M.bindings=[findCardsD <--> findCards]})
            ]


natEditor :: M.Component a Natural Natural
natEditor = M.component 0 M.put view
    where view n = H.input_ [
            P.type_ "number",
            P.min_ "0",
            P.value_ (M.toMisoString $ show n),
            H.onChange (read . M.fromMisoString )
            ]

chooseEditor :: (Eq parent) => a -> M.Component (NE.NonEmpty a) parent b -> Lens parent a -> M.Component parent (NE.NonEmpty a) (Maybe Int)
chooseEditor def editor l  = M.component (def NE.:| []) update view 
    where 
        update Nothing = M.modify $ NE.cons def
        update (Just i) = do
            (e NE.:| es) <- M.get
            let es' = take i es ++ drop (i+1) es
            M.put (e NE.:| es')
        headLens = lens NE.head $ \(_ NE.:| es) e -> e NE.:| es
        bodyLens i = let 
            get (_ NE.:| es) = fromMaybe def $ es !? i
            set (e NE.:| es) e' = let 
                es' = take i es ++  (e':drop (i+1) es)
                in e NE.:| es'
            in lens get set
        view (_ NE.:| es) = let
                headHtml = H.div_ [] +> editor{M.bindings=[headLens <--> l]}
                bodyHtml i = H.div_[] [
                   H.span_ [] +> editor{M.bindings=[bodyLens i <--> l]},
                   H.button_ [H.onClick (Just i)] [text "-"]
                   ]
                addBtn = H.button_ [H.onClick Nothing] [text "+"]
                in H.div_ [] $ addBtn:headHtml:map bodyHtml [0..length es-1]


optionalEditor :: (Eq parent) => a -> M.Component a parent c -> Lens parent a -> M.Component parent a b
optionalEditor def editor l = 
    let 
        view = H.span_ [] +> editor{M.bindings=[noLens <--> l]}
    in  M.component def M.noop  (const view)

atomEditor :: a -> (Int,a->a) -> [(M.MisoString,M.MisoString,Bound (Int,a))] -> M.Component parent (Int,a) Int
atomEditor def specCase info = M.component (0,def) update view
    where 
        update i = do
            M.modify $ first $ const i
            -- Special case for opitionals 
            when (i == fst specCase ) $ M.modify $ second $ const $ snd specCase def
        view (i,_) = H.span_ [] [
                H.select_ [
                    H.onChange getIndex ,
                    P.value_ $ f $ info !! i
                ] (map mkOption info),
                map mkEditor info !! i
            ]
        getIndex s = fromMaybe 0 $ findIndex ((==) s . f) info
        mkOption (n,k,_) = H.option_ [P.value_ k] [text n]
        mkEditor (_,k,comp) = M.node M.HTML k [] [comp (H.span_ []) ]
        f (_,x,_) = x

noLens = lens id (\x y -> y)

bind l component b = flip M.mount $ component {M.bindings=[l <--> b]}

type Bound a = ([M.View a Int] -> M.View a Int) -> M.View a Int

