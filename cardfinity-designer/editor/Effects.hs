{-# LANGUAGE TemplateHaskell #-}

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

class ToEffect a where
    toEffect :: a -> A.Effect

instance ToEffect (A.DestroyType, A.FindCards) where
    toEffect = uncurry DestroyEnemy

data DiscardEnemyModel = DiscardEnemyModel
instance ToEffect DiscardEnemyModel where
    toEffect = const DiscardEnemy

instance ToEffect (Natural, Bool) where
    toEffect  = uncurry DealDamage

newtype HealModel = HealModel Natural
instance ToEffect HealModel where
    toEffect (HealModel n) = Heal n

data DECKOUTModel = DECKOUTModel
instance ToEffect DECKOUTModel where
    toEffect = const DECKOUT

newtype DrawModel = DrawModel Natural
instance ToEffect DrawModel where
    toEffect (DrawModel n) = Draw n

data PeekScryModel = PeekModel Natural | ScryModel Natural
instance ToEffect PeekScryModel where
    toEffect (PeekModel n) = Peek n
    toEffect (ScryModel n) = Scry n

newtype OptionalModel = OptionalModel EffectModel
instance ToEffect OptionalModel where
    toEffect (OptionalModel (EffectModel em)) = Optional $ toEffect em

newtype ChooseModel = ChooseModel (NE.NonEmpty EffectModel)
instance ToEffect ChooseModel where
    toEffect (ChooseModel ems) = ChooseEffect $ NE.map h ems
        where h (EffectModel e) = toEffect e

newtype AttackModel = AttackModel Bool
instance ToEffect AttackModel where
    toEffect (AttackModel b) = Attack b

newtype PlayModel = PlayModel A.SearchType
instance ToEffect PlayModel where
    toEffect  (PlayModel s) = Play s

newtype SearchModel = SearchModel A.SearchMethod
instance ToEffect SearchModel where
    toEffect (SearchModel m) = Search m

newtype AttachModel = AttachModel A.SearchType
instance ToEffect AttachModel where
    toEffect  (AttachModel s) = Attach s

instance ToEffect (Integer, Bool) where
    toEffect = uncurry Buff

-- TODO: AsEffect

data EffectModel = forall a . ToEffect a => EffectModel a
