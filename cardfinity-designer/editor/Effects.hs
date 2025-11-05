{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

module Effects (effectEditor,effect ) where

import qualified Atoms as A
import qualified Miso as M
import qualified Miso.Html as H
import qualified Miso.Html.Property as P
import Miso.Lens (Lens (_get, _set),lens)
import Miso ((<-->), (+>), text)
import GHC.Natural (Natural)
import qualified Data.List.NonEmpty as NE (NonEmpty ((:|)))
import Shared 
import Data.Bifunctor (second, first)
import Conditions (conditionEditor, asEffect )

destroy :: Lens DestroyModel A.Effect
destroy = let
    get (DestroyM b fc) = flip A.DestroyEnemy fc $ if b then A.Banish else A.Discard
    set m = \case
        A.DestroyEnemy A.Discard fc -> DestroyM False fc
        A.DestroyEnemy A.Banish fc -> DestroyM True fc
        _ -> m
    in lens get set

discard :: Lens () A.Effect
discard = lens (const A.DiscardEnemy) (const (const ()))

discardEditor = M.component () M.noop (const $ text "")

dealDamage :: Lens (Natural, Bool) A.Effect
dealDamage = let
    get  = uncurry A.DealDamage
    unget (A.DealDamage n b)  = (n,b)
    unget _ = (0,False)
    in lens get (const unget)

heal :: Lens Natural A.Effect
heal = lens A.Heal $ const $ \case
    A.Heal n -> n
    _ -> 0

draw :: Lens Natural A.Effect
draw = lens A.Draw $ const $ \case
    A.Draw n -> n
    _ -> 0

peek :: Lens Natural A.Effect
peek = lens A.Peek $ const $ \case
    A.Peek n -> n
    _ -> 0

scry :: Lens Natural A.Effect
scry = lens A.Scry $ const $ \case
    A.Scry n -> n
    _ -> 0

deckout :: Lens () A.Effect
deckout = lens (const A.DECKOUT) (const (const ()))

deckoutEditor = M.component () M.noop (const $ text "")

optionalEffect =let
   set _ (A.Optional e) = setEffect  e
   set ie _ = ie
   in lens (A.Optional . snd) set

chooseEffect :: Lens (NE.NonEmpty A.Effect) A.Effect
chooseEffect = lens A.ChooseEffect $ const $ \case 
    A.ChooseEffect es -> es
    _ -> A.DiscardEnemy NE.:| []

chooseEffectEditor = neListEditor A.DiscardEnemy effect effectEditor 

attack :: Lens Bool A.Effect
attack = lens A.Attack $ const $ \case 
    A.Attack b -> b
    _ -> False

attackEditor = M.component False (const $ M.modify not) view
    where view = const $ H.button_ [H.onClick ()] [text "Toggle"]

play :: Lens A.SearchType A.Effect
play = lens A.Play $ const $ \case
    A.Play st -> st
    _ -> A.ForCard

attach :: Lens A.SearchType A.Effect
attach = lens A.Attach $ const $ \case
    A.Attach st -> st
    _ -> A.ForCard

stEditor = M.component A.ForCard M.noop view
    where 
        view :: A.SearchType -> M.View A.SearchType ()
        view = const $ H.span_ [] +> (searchTypeEditor {M.bindings=[noLens <-->searchType ]})

search :: Lens (Bool, A.SearchType) A.Effect
search = let 
    get (d,t) = (A.Search . (if d then A.DrillFor else A.SearchFor)) t
    set = const $ \case 
        A.Search (A.SearchFor t) -> (,) False t
        A.Search (A.DrillFor t) -> (,) True t
        _ -> (,) False A.ForCard
    in lens get set

searchEditor = M.component (False, A.ForCard) (const $ M.modify (first not)) view
    where 
    sndLens = lens snd $ \(a,_) b -> (a,b) 
    view = const $ H.span_ [] [
        H.button_ [H.onClick ()] [text "Toggle"],
        H.span_ [] +> (searchTypeEditor {M.bindings=[sndLens<-->searchType ]})
        ]

buff :: Lens (Integer,Bool) A.Effect
buff = lens (uncurry A.Buff) $ const $ \case
    A.Buff x b -> (x,b)
    _ -> (0,False)

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

info :: [(M.MisoString, M.MisoString,Bound (Int,A.Effect))]
info = [
    ("Discard","discard", bind effect discardEditor discard),
    ("Destroy","destroy", bind effect destroyEditor destroy),
    ("Deal Damage","dealdamage", bind effect damageEditor dealDamage),
    ("Heal","heal", bind effect natEditor heal),
    ("DECKOUT", "deckout", bind effect deckoutEditor deckout),
    ("Draw","draw",bind effect natEditor draw),
    ("Peek","peek",bind effect natEditor peek),
    ("Scry","scry",bind effect natEditor scry),
    ("Optional","optional",bind effect effectEditor optionalEffect),
    ("Choose","choose",bind effect chooseEffectEditor chooseEffect),
    ("Attack", "attack", bind effect attackEditor attack),
    ("Play", "play", bind effect stEditor play),
    ("Search", "search", bind effect searchEditor search),
    ("Attach", "attach", bind effect stEditor attach),
    ("Buff", "buff", bind effect buffEditor buff),
    ("As Effect", "aseffect", bind effect conditionEditor asEffect)
    ]

-- Must match the order of `info`
setEffect = \case           
    A.DiscardEnemy ->  (,) 0 A.DiscardEnemy
    e@(A.DestroyEnemy _ _) -> (,) 1 e 
    e@(A.DealDamage _ _) -> (,) 2 e
    e@(A.Heal _) -> (,) 3 e
    A.DECKOUT -> (,) 4 A.DECKOUT
    e@(A.Draw _) -> (,) 5 e
    e@(A.Peek _) -> (,) 6 e
    e@(A.Scry _) -> (,) 7 e
    e@(A.Optional _) -> (,) 8 e
    e@(A.ChooseEffect _) -> (,) 9 e
    e@(A.Attack _) -> (,) 10 e
    e@(A.Play _) -> (,) 11 e
    e@(A.Search _) -> (,) 12 e
    e@(A.Attach _) -> (,) 13 e
    e@(A.Buff _ _) -> (,) 14 e
    e@(A.AsEffect _) -> (,) 15 e

effect = lens snd (const setEffect)
effectEditor = atomEditor A.DiscardEnemy info
