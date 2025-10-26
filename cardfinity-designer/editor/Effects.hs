{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

module Effects (effectEditor,effect ) where

import qualified Atoms as A
import qualified Miso as M
import qualified Miso.Html as H
import qualified Miso.Html.Property as P
import Miso.Lens (Lens (_get, _set),lens, (.=), (%=), (^.))
import Miso.Lens.TH (makeLenses)
import Miso ((<-->), (+>), text)
import Atoms (Effect(..))
import GHC.Natural (Natural)
import qualified Data.List.NonEmpty as NE (NonEmpty ((:|)), map,cons, head, tail)
import Shared 
import Data.Maybe (fromMaybe)
import Data.List (findIndex, (!?))
import Data.Function ((&))
import Data.Bifunctor (second, first)
import Numeric (readInt)
import qualified GHC.Generics as M
import Control.Monad (when)

data Model = EffectEditorModel {
    _effectIdx :: Int,
    _effect' :: Effect
} deriving (Eq)

$(makeLenses ''Model)

destroy :: Lens DestroyModel Effect
destroy = let
    get (DestroyM b fc) = flip DestroyEnemy fc $ if b then A.Banish else A.Discard
    set m = \case
        DestroyEnemy A.Discard fc -> DestroyM False fc
        DestroyEnemy A.Banish fc -> DestroyM True fc
        _ -> m
    in lens get set

discard :: Lens () Effect
discard = lens (const DiscardEnemy) (const (const ()))

discardEditor :: M.Component Model () ()
discardEditor = M.component () M.noop (const $ text "")

dealDamage :: Lens (Natural, Bool) Effect
dealDamage = let
    get  = uncurry A.DealDamage
    unget (A.DealDamage n b)  = (n,b)
    unget _ = (0,False)
    in lens get (const unget)

heal :: Lens Natural Effect
heal = lens A.Heal $ const $ \case
    A.Heal n -> n
    _ -> 0

draw :: Lens Natural Effect
draw = lens A.Draw $ const $ \case
    A.Draw n -> n
    _ -> 0

peek :: Lens Natural Effect
peek = lens A.Peek $ const $ \case
    A.Peek n -> n
    _ -> 0

scry :: Lens Natural Effect
scry = lens A.Scry $ const $ \case
    A.Scry n -> n
    _ -> 0

deckout :: Lens () Effect
deckout = lens (const DECKOUT) (const (const ()))

deckoutEditor :: M.Component Model () ()
deckoutEditor = M.component () M.noop (const $ text "")

optionalEffectEditor = optionalEditor A.DiscardEnemy effectEditor optionalEffect
optionalEffect = lens (A.Optional . _get effect) $ const $ \case
    A.Optional e -> setEffect e
    _ -> EffectEditorModel 0 A.DiscardEnemy

chooseEffect :: Lens (NE.NonEmpty Effect) Effect
chooseEffect = lens A.ChooseEffect $ const $ \case 
    A.ChooseEffect es -> es
    _ -> A.DiscardEnemy NE.:| []

chooseEffectEditor = chooseEditor A.DiscardEnemy effectEditor effect

attack :: Lens Bool Effect
attack = lens A.Attack $ const $ \case 
    A.Attack b -> b
    _ -> False

attackEditor :: M.Component Model Bool ()
attackEditor = M.component False (const $ M.modify not) view
    where view = const $ H.button_ [H.onClick ()] [text "Toggle"]

play :: Lens A.SearchType Effect
play = lens A.Play $ const $ \case
    A.Play st -> st
    _ -> A.ForCard

attach :: Lens A.SearchType Effect
attach = lens A.Attach $ const $ \case
    A.Attach st -> st
    _ -> A.ForCard

stEditor :: M.Component Model A.SearchType ()
stEditor = M.component A.ForCard M.noop view
    where 
        view :: A.SearchType -> M.View A.SearchType ()
        view = const $ H.span_ [] +> (searchTypeEditor {M.bindings=[noLens <-->searchType ]})

search :: Lens (Bool, A.SearchType) Effect
search = let 
    get (d,t) = (A.Search . (if d then A.DrillFor else A.SearchFor)) t
    set = const $ \case 
        A.Search (A.SearchFor t) -> (,) False t
        A.Search (A.DrillFor t) -> (,) True t
        _ -> (,) False A.ForCard
    in lens get set

searchEditor :: M.Component Model (Bool,A.SearchType) ()
searchEditor = M.component (False, A.ForCard) (const $ M.modify (first not)) view
    where 
    sndLens = lens snd $ \(a,_) b -> (a,b) 
    view = const $ H.span_ [] [
        H.button_ [H.onClick ()] [text "Toggle"],
        H.span_ [] +> (searchTypeEditor {M.bindings=[sndLens<-->searchType ]})
        ]

buff :: Lens (Integer,Bool) Effect
buff = lens (uncurry A.Buff) $ const $ \case
    A.Buff x b -> (x,b)
    _ -> (0,False)

buffEditor :: M.Component Model (Integer, Bool) (Maybe Integer)
buffEditor = M.component (0,False) update view
    where
        update (Just i) = M.modify $ first $ const i
        update Nothing = M.modify $ second not
        view (i,_) = H.span_ [] [
            H.input_ [
                P.type_ "number",
                P.value_ (M.toMisoString $ show  i),
                H.onChange (Just . read . M.fromMisoString )
            ],
            H.button_ [H.onClick Nothing] [text "Toggle"]
            ]

asEffect :: Lens A.Condition Effect
asEffect = lens A.AsEffect $ const $ \case
    A.AsEffect c -> c
    _ -> A.DiscardSelf

asEffectEditor :: M.Component Model A.Condition ()
asEffectEditor = M.component A.DiscardSelf M.noop view
    where view _ = text "TODO"

info :: [(M.MisoString, M.MisoString,Bound Model)]
info = [
    ("Discard","discard", bind effect discardEditor discard),
    ("Destroy","destroy", bind effect destroyEditor destroy),
    ("Deal Damage","dealdamage", bind effect damageEditor dealDamage),
    ("Heal","heal", bind effect natEditor heal),
    ("DECKOUT", "deckout", bind effect deckoutEditor deckout),
    ("Draw","draw",bind effect natEditor draw),
    ("Peek","peek",bind effect natEditor peek),
    ("Scry","scry",bind effect natEditor scry),
    ("Optional","optional",bind effect optionalEffectEditor noLens),
    ("Choose","choose",bind effect chooseEffectEditor chooseEffect),
    ("Attack", "attack", bind effect attackEditor attack),
    ("Play", "play", bind effect stEditor play),
    ("Search", "search", bind effect searchEditor search),
    ("Attach", "attach", bind effect stEditor attach),
    ("Buff", "buff", bind effect buffEditor buff),
    ("As Effect", "aseffect", bind effect asEffectEditor asEffect )
    ]

-- Must match the order of `info`
setEffect = \case           
    A.DiscardEnemy ->  EffectEditorModel 0 A.DiscardEnemy
    e@(A.DestroyEnemy _ _) -> EffectEditorModel 1 e 
    e@(A.DealDamage _ _) -> EffectEditorModel 2 e
    e@(A.Heal _) -> EffectEditorModel 3 e
    A.DECKOUT -> EffectEditorModel 4 A.DECKOUT
    e@(A.Draw _) -> EffectEditorModel 5 e
    e@(A.Peek _) -> EffectEditorModel 6 e
    e@(A.Scry _) -> EffectEditorModel 7 e
    e@(A.Optional _) -> EffectEditorModel 8 e
    e@(A.ChooseEffect _) -> EffectEditorModel 9 e
    e@(A.Attack _) -> EffectEditorModel 10 e
    e@(A.Play _) -> EffectEditorModel 11 e
    e@(A.Search _) -> EffectEditorModel 12 e
    e@(A.Attach _) -> EffectEditorModel 13 e
    e@(A.Buff _ _) -> EffectEditorModel 14 e
    e@(A.AsEffect _) -> EffectEditorModel 15 e

effect = lens _effect' (const setEffect )

effectEditor :: M.Component parentModel  Model Int
effectEditor = M.component def update view
    where
        def = EffectEditorModel 0 DiscardEnemy
        update t =do
            effectIdx .= t
            -- When setting to optional: set the effect manually
            when (t == 8) $ effect' .= A.Optional A.DiscardEnemy
        view m = H.span_ [] [
            H.select_ [
                H.onChange getIndex,
                P.value_ (f $ info !! (m^.effectIdx ))
                ] (map mkOption info),
            map mkEditor info !! (m ^. effectIdx)
         ]
        getIndex s = fromMaybe 0 $ findIndex ((==) s . f) info
        mkOption (n,k,_) = H.option_ [P.value_ k] [text n]
        mkEditor (_,k,comp) = M.node M.HTML k [] [comp (H.span_ []) ]
        f (_,x,_) = x
