{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

module Effects (effectEditor,effect ) where

import qualified Atoms as A
import qualified Miso as M
import qualified Miso.Html as H
import qualified Miso.Html.Property as P
import Miso.Lens (Lens,lens, (.=), (%=), (^.))
import Miso.Lens.TH (makeLenses)
import Miso ((<--), (+>), text)
import Atoms (Effect(..))
import GHC.Natural (Natural)
import qualified Data.List.NonEmpty as NE (NonEmpty ((:|)), map)
import Shared (findCardsEditor,findCards )
import Data.Maybe (fromMaybe)
import Data.List (findIndex)
import Data.Function ((&))



data Model = EffectEditorModel {
    _effectName :: Int,
    _effect :: Effect
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
            H.span_ [] +> (findCardsEditor {M.bindings=[findCardsD <-- findCards]})
            ]


discard :: Lens () Effect
discard = lens (const DiscardEnemy) (const (const ())) 

discardEditor :: M.Component Model () ()
discardEditor = M.component () M.noop (const $ text "")

bind component b = flip (+>) $ component {M.bindings=[effect <-- b]}

type Bound = ([M.View Model Int] -> M.View Model Int) -> M.View Model Int

info :: [(M.MisoString,Bound)]
info = [
    ("Destroy", bind destroyEditor destroy),
    ("Discard", bind discardEditor discard)
    ]

effectEditor :: M.Component parentModel  Model Int
effectEditor = M.component def update view 
    where 
        def = EffectEditorModel 0 DiscardEnemy
        update t = effectName .= t
        view m = H.span_ [] [ 
            H.select_ [H.onChange effIndex ] (map mkOption info),
            map mkEditor info !! (m ^. effectName)
         ]
        effIndex s = fromMaybe 0 $ findIndex ((==) s . fst) info
        -- z m = 
        mkOption (k,_) = H.option_ [P.value_ k] [text k]
        mkEditor (k,comp) = M.node M.HTML k [] [comp (H.span_ []) ]

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
