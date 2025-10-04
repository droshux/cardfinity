{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE AllowAmbiguousTypes #-}

module Effects (effectEditor ) where

import qualified Atoms as A
import qualified Miso as M
import Miso.Lens (Lens,lens)
import Miso.Lens.TH (makeLenses)
import Atoms (Effect(..))
import GHC.Natural (Natural)
import qualified Data.List.NonEmpty as NE (NonEmpty ((:|)), map)



newtype Model = EffectEditorModel {
    _effect :: A.Effect
}

defEffectEditor = EffectEditorModel A.DECKOUT

$(makeLenses ''Model)

effectEditor :: M.Component parentModel  Model ()
effectEditor = M.component defEffectEditor _ _

class EffectModel a b where
    toEffect :: a -> A.Effect
    editor :: M.Component Model a b

instance EffectModel (A.DestroyType, A.FindCards) () where
    toEffect = uncurry DestroyEnemy

data DiscardEnemyModel = DiscardEnemyModel
instance EffectModel DiscardEnemyModel () where
    toEffect = const DiscardEnemy

instance EffectModel (Natural, Bool) () where
    toEffect  = uncurry DealDamage

newtype HealModel = HealModel Natural
instance EffectModel HealModel () where
    toEffect (HealModel n) = Heal n

data DECKOUTModel = DECKOUTModel
instance EffectModel DECKOUTModel () where
    toEffect = const DECKOUT

newtype DrawModel = DrawModel Natural
instance EffectModel DrawModel () where
    toEffect (DrawModel n) = Draw n

data PeekScryModel = PeekModel Natural | ScryModel Natural
instance EffectModel PeekScryModel () where
    toEffect (PeekModel n) = Peek n
    toEffect (ScryModel n) = Scry n

newtype OptionalModel = OptionalModel IsEffectModel
instance EffectModel OptionalModel () where
    toEffect (OptionalModel (EffectModel em)) = Optional $ toEffect em

newtype ChooseModel = ChooseModel (NE.NonEmpty IsEffectModel)
instance EffectModel ChooseModel () where
    toEffect (ChooseModel ems) = ChooseEffect $ NE.map h ems
        where h (EffectModel e) = toEffect e

newtype AttackModel = AttackModel Bool
instance EffectModel AttackModel () where
    toEffect (AttackModel b) = Attack b

newtype PlayModel = PlayModel A.SearchType
instance EffectModel PlayModel () where
    toEffect  (PlayModel s) = Play s

newtype SearchModel = SearchModel A.SearchMethod
instance EffectModel SearchModel () where
    toEffect (SearchModel m) = Search m

newtype AttachModel = AttachModel A.SearchType
instance EffectModel AttachModel () where
    toEffect  (AttachModel s) = Attach s

instance EffectModel (Integer, Bool) () where
    toEffect = uncurry Buff

-- TODO: AsEffect

data IsEffectModel = forall a b . EffectModel a b => EffectModel a
