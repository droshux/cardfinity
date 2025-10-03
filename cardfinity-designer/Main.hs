{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Atoms qualified as A
import Data.Either (fromRight)
import GHC.Natural (Natural)
import Miso (text)
import Miso qualified as M
import Miso.Html qualified as H
import Miso.Lens
import Scale (HasScale (scale), Scale, runScale)

main :: IO ()
main = M.run (M.startApp app)

#ifdef WASM
foreign export javascript "hs_start" main :: IO ()
#endif

data Model = Model
  { _damage :: Natural,
    _isTrue :: Bool
  }
  deriving (Eq)

damage = lens _damage $ \m d -> m {_damage = d}

isTrue = lens _isTrue $ \m t -> m {_isTrue = t}

data Action = Up | Down | Reset | Toggle

app :: M.App Model Action
app = M.component (Model 0 False) updateModel viewModel

updateModel :: Action -> M.Transition Model Action
updateModel Reset = damage .= 0
updateModel Up = damage += 1
updateModel Down = damage %= \d -> max d 1 - 1
updateModel Toggle = isTrue %= not

viewModel :: Model -> M.View Model Action
viewModel (Model dmg isTrue) =
  H.div_
    []
    [ H.div_
        []
        [ H.button_ [H.onClick Up] [text "+"],
          H.button_ [H.onClick Reset] [text "reset"],
          H.button_ [H.onClick Down] [text "-"],
          H.button_ [H.onClick Toggle] [text "Toggle Truth"]
        ],
      let takeDmg = A.TakeDamage dmg isTrue
       in H.div_
            []
            [ H.p_ [] [(text . M.toMisoString . show) takeDmg],
              H.p_
                []
                [ text "Scale: ",
                  (text . M.toMisoString . calcScale) takeDmg
                ]
            ]
    ]

calcScale :: (HasScale a) => a -> Int
calcScale = fromRight 0 . runScale []
