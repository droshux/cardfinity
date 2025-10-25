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
import qualified Data.List.NonEmpty as NE (NonEmpty ((:|)), map)
import Shared (findCardsEditor,findCards )
import Data.Maybe (fromMaybe)
import Data.List (findIndex)
import Data.Function ((&))
import Data.Bifunctor (second, first)
import Numeric (readInt)
import qualified GHC.Generics as M



data Model = EffectEditorModel {
    _effectIdx :: Int,
    _effect' :: Effect
} deriving (Eq)

$(makeLenses ''Model)

data GenericActions = Toggle | Inc | Dec

data DestroyModel = DestroyM {
    _banish :: Bool,
    _findCardsD :: A.FindCards
} deriving (Eq)

$(makeLenses ''DestroyModel)

destroy :: Lens DestroyModel Effect
destroy = let
    get (DestroyM b fc) = flip DestroyEnemy fc $ if b then A.Banish else A.Discard
    set m = \case
        DestroyEnemy A.Discard fc -> DestroyM False fc
        DestroyEnemy A.Banish fc -> DestroyM True fc
        _ -> m
    in lens get set

destroyEditor :: M.Component Model DestroyModel GenericActions
destroyEditor = M.component def update view
    where
        def =  DestroyM False $ A.FindCardsHand 0 A.ForCard
        update Toggle = banish %= not
        update _ = return ()
        view _ = H.span_ [] [
            H.button_ [H.onClick Toggle] [text "Banish"],
            H.span_ [] +> (findCardsEditor {M.bindings=[findCardsD <--> findCards]})
            ]


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

dealDamageEditor :: M.Component Model (Natural,Bool) (Maybe Natural)
dealDamageEditor = M.component (0,False) update view
    where update Nothing = M.modify (second not)
          update (Just n) = M.modify (first (const n))
          view _ = H.span_ [] [
            H.button_ [H.onClick Nothing] [text "Toggle"],
            H.input_ [
                P.type_ "number",
                P.min_ "0",
                H.onChange (Just . read . M.fromMisoString )
            ]
              ]

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

natEditor :: M.Component Model Natural Natural
natEditor = M.component 0 M.put view
    where view =const $ H.input_ [
            P.type_ "number",
            P.min_ "0",
            H.onChange (read . M.fromMisoString )
            ]

deckout :: Lens () Effect
deckout = lens (const DECKOUT) (const (const ()))

deckoutEditor :: M.Component Model () ()
deckoutEditor = M.component () M.noop (const $ text "")

newtype OptionalModel = OM {
    _optionalEffect :: Effect
} deriving Eq

$(makeLenses ''OptionalModel)

optionalEditor :: M.Component Model OptionalModel ()
optionalEditor  = M.component (OM A.DiscardEnemy) M.noop view
    where view _ = H.span_ [] +> (effectEditor {M.bindings=[optionalEffect<-->mkOptional effect]})
          mkOptional l = lens (A.Optional . _get l) $ const $ \case
            A.Optional e -> setEffect e
            _ -> EffectEditorModel 0 A.DiscardEnemy

bind component b = flip M.mount $ component {M.bindings=[effect <--> b]}

type Bound = ([M.View Model Int] -> M.View Model Int) -> M.View Model Int

info :: [(M.MisoString, M.MisoString,Bound)]
info = [
    ("Discard","discard", bind discardEditor discard),
    ("Destroy","destroy", bind destroyEditor destroy),
    ("Deal Damage","dealdamage", bind dealDamageEditor dealDamage),
    ("Heal","heal", bind natEditor heal),
    ("DECKOUT", "deckout", bind deckoutEditor deckout),
    ("Draw","draw",bind natEditor draw),
    ("Peek","peek",bind natEditor peek),
    ("Scry","scry",bind natEditor scry),
    ("Optional","optional",bind optionalEditor optionalEffect)
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
        update t = effectIdx .= t
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
{- data Effect
  = DestroyEnemy DestroyType FindCards
  | DiscardEnemy
  | DealDamage Natural Bool
  | Heal Natural
  | DECKOUT
  | Draw Natural
  | Peek Natural
  | Scry Natural
  | Optional Effect
  | ChooseEffect (NonEmpty Effect)
  | Attack Bool
  | Play SearchType
  | Search SearchMethod
  | Attach SearchType
  | Buff Integer Bool
  | AsEffect Condition
  deriving (Eq, Ord) -}
