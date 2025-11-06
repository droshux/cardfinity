{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Editor.Conditions (condition, conditionEditor, asEffect) where

import Atoms qualified as A
import Control.Monad (when)
import Data.List.NonEmpty qualified as NE
import GHC.Natural (Natural)
import Miso qualified as M
import Miso.Html qualified as H
import Miso.Html.Property qualified as P
import Miso.Lens (Lens, lens)
import Editor.Shared

discard = lens (const A.DiscardSelf) (const (const ()))

discardEditor = M.component () M.noop (const $ M.text "")

destroy :: Lens DestroyModel A.Condition
destroy =
  let get (DestroyM b fc) = flip A.Destroy fc $ if b then A.Banish else A.Discard
      set = const $ \case
        A.Destroy A.Discard fc -> DestroyM False fc
        A.Destroy A.Banish fc -> DestroyM True fc
        _ -> DestroyM False $ A.FindCardsHand 0 A.ForCard
   in lens get set

takeDamage :: Lens (Natural, Bool) A.Condition
takeDamage = lens (uncurry A.TakeDamage) $ const $ \case
  A.TakeDamage n b -> (n, b)
  _ -> (0, False)

healOpponent :: Lens Natural A.Condition
healOpponent = lens A.HealOpponent $ const $ \case
  A.HealOpponent n -> n
  _ -> 0

pop :: Lens Natural A.Condition
pop = lens A.Pop $ const $ \case
  A.Pop n -> n
  _ -> 0

youMay =
  let set _ (A.YouMay c) = setCondition c
      set ic _ = ic
   in lens (A.YouMay . snd) set

chooseCond :: Lens (NE.NonEmpty A.Condition) A.Condition
chooseCond = lens A.Choose $ const $ \case
  A.Choose cs -> cs
  _ -> A.DiscardSelf NE.:| []

chooseCondEdior = neListEditor A.DiscardSelf condition conditionEditor

info :: [(M.MisoString, M.MisoString, Bound (Int, A.Condition))]
info =
  [ ("Discard", "discard", bind condition discardEditor discard),
    ("Destroy", "destroy", bind condition destroyEditor destroy),
    ("Take Damage", "takedamage", bind condition damageEditor takeDamage),
    ("Heal Opponent", "heal", bind condition natEditor healOpponent),
    ("Pop", "pop", bind condition natEditor pop),
    ("You May", "youmay", bind condition conditionEditor youMay),
    ("Choose", "choose", bind condition chooseCondEdior chooseCond)
  ]

-- Must match the order of `info`
setCondition = \case
  A.DiscardSelf -> (0, A.DiscardSelf)
  c@(A.Destroy _ _) -> (1, c)
  c@(A.TakeDamage _ _) -> (2, c)
  c@(A.HealOpponent _) -> (3, c)
  c@(A.Pop _) -> (4, c)
  c@(A.YouMay _) -> (5, c)
  c@(A.Choose _) -> (6, c)

condition = lens snd (const setCondition)

conditionEditor = atomEditor A.DiscardSelf info

asEffect :: Lens (Int, A.Condition) A.Effect
asEffect = lens (A.AsEffect . snd) $ const $ \case
  A.AsEffect c -> setCondition c
  _ -> setCondition A.DiscardSelf

