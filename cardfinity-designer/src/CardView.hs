{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module CardView (cardView, CardViewProps (CardViewProps)) where

import Atoms qualified as CF
import Data.Foldable (Foldable (toList))
import Data.List (intersperse)
import Data.Maybe (fromMaybe, isNothing)
import Data.Set.Ordered qualified as OS
import Miso qualified as M
import Miso.CSS qualified as CSS
import Miso.Html qualified as H
import Miso.Html.Property qualified as P
import Miso.Lens (Lens, lens)
import Miso.Lens qualified as M
import Miso.Lens.TH (makeLenses)
import Optics.Operators ((^.))
import Scale (runScale)
import Shared qualified
import ShowCard (show'Spell)
import Types qualified as CF
import Utils qualified as CF

data CardViewProps = CardViewProps
  { _card :: CF.Card,
    _deck :: [CF.Card]
  }
  deriving (Eq)

$(makeLenses ''CardViewProps)

data CardViewModel = CardViewModel
  { _conciseView :: Bool,
    _printView :: Bool
  }
  deriving (Eq)

$(makeLenses ''CardViewModel)

data CardViewAction = ToggleCode | ToggleConcise | TogglePrint

cardView :: M.Component ctx CardViewProps CardViewModel CardViewAction
cardView = M.component modelDefault update view
  where
    modelDefault = CardViewModel False False
    update ToggleConcise = conciseView M.%= not
    update TogglePrint = printView M.%= not

view :: ctx -> CardViewProps -> CardViewModel -> M.View ctx CardViewAction
view _ props m =
  H.div_
    []
    [ H.span_
        []
        [ H.button_ [H.onClick ToggleConcise] [M.text $ if m M.^. conciseView then "Full" else "Concise"]
        ],
      -- TODO: print view
      if m M.^. printView
        then M.text "TODO: display entire deck for printing"
        else viewCard props m,
      H.pre_ [] [M.text $ M.toMisoString $ CF.unparse (m M.^. conciseView) (props M.^. card)]
    ]

viewCard :: CardViewProps -> CardViewModel -> M.View ctx CardViewAction
viewCard props m =
  H.div_
    [ CSS.style_
        [ CSS.aspectRatio "2.5 / 3.5",
          CSS.border "solid 1px black",
          CSS.maxWidth $ if m M.^. printView then "2.5in" else CSS.vw 40,
          CSS.maxHeight $ if m M.^. printView then "3.5in" else CSS.vh 56,
          CSS.display "flex",
          CSS.flexDirection "column",
          CSS.paddingLeft $ CSS.pct 2.5,
          CSS.paddingRight $ CSS.pct 2.5,
          CSS.gap $ CSS.pct 0.5
        ]
    ]
    [ -- Top Row
      H.span_
        []
        [ H.span_ [CSS.style_ [CSS.marginBottom $ CSS.pct 0.5]] [showScale (props M.^. deck) (props M.^. card)],
          H.span_
            [ CSS.style_ [CSS.marginLeft $ CSS.em 1]
            ]
            [showName (props M.^. card)]
        ],
      showImage $ props M.^. card ^. CF.cardImageUrl,
      H.div_ [CSS.style_ [CSS.display "flex", CSS.justifyContent "space-evenly"]] (map (H.em_ [] . (: []) . M.text . M.toMisoString) $ toList $ props M.^. card ^. CF.cardFamilies),
      CF.cardStatsElim (showSpell False $ m M.^. conciseView) (showMonster $ m M.^. conciseView) (props M.^. card ^. CF.cardStats)
    ]

showName :: CF.Card -> M.View model action
showName c =
  let name = CF.cardName c
   in if name == "" then H.em_ [] [M.text "No Name"] else M.text $ M.toMisoString name

showScale :: [CF.Card] -> CF.Card -> M.View model action
showScale deck = M.text . either (const "?") M.toMisoString . runScale deck

showImage :: Maybe String -> M.View model action
showImage mbUrl =
  H.img_
    [ P.src_ $ maybe "assets/icons/snail.svg" M.toMisoString mbUrl, -- TODO: Replace snail with proper placeholder
      CSS.style_ $ CSS.aspectRatio "2.5 / 2" : [CSS.width "100%" | isNothing mbUrl]
    ]

showText :: Bool -> CF.CardText -> [M.View model action]
showText False (CF.Text t) = [M.text $ M.toMisoString t]
showText True (CF.Text t) = [M.text $ M.toMisoString " "]
showText _ (CF.Number n) = [M.text $ M.toMisoString $ show n]
showText _ (CF.Trigger t) = [Shared.triggerIcon t]
showText _ (CF.CardName f) = [M.text $ M.toMisoString $ show f]
showText False (CF.CardFamily f) = [M.text $ M.toMisoString $ show f]
showText True (CF.CardFamily f) = [M.text $ M.toMisoString $ 'f' : show f]
showText _ (CF.Keyword w) = [H.em_ [] [M.text $ M.toMisoString w]]
showText c (CF.Branch CF.Indent r) = [H.span_ [CSS.style_ [CSS.textIndent "2em"]] $ showText c r]
showText c (CF.Branch l r) = showText c l ++ showText c r
showText c (CF.List xs) = [H.ul_ [] $ flip map (toList xs) $ \x -> H.li_ [] (showText c x)]
showText _ CF.NewLine = [H.br_ []]
showText _ CF.Indent = []
showText c (CF.Copies t n) =
  [ H.div_
      [ CSS.style_ [CSS.display "inline-flex"]
      ]
      [ H.span_ [CSS.style_ [CSS.flexGrow 1.0, CSS.overflowX "wrap"]] (showText c t),
        H.span_ [] [M.text $ M.toMisoString $ show n]
      ]
  ]

showSpell :: Bool -> Bool -> CF.Spell -> M.View model action
showSpell name c = H.span_ [] . showText c . show'Spell name

showMonster :: Bool -> CF.Monster -> M.View model action
showMonster c m =
  H.div_
    [ CSS.style_
        [ CSS.display "flex",
          CSS.flexDirection "column",
          CSS.justifyContent "space-between",
          CSS.height "100%"
        ]
    ]
    [ H.span_ [] (showText c $ mconcat $ intersperse (CF.txt ", ") $ map CF.show' $ toList $ m ^. CF.summoningConditions),
      M.vfrag $ flip map (CF.collapse $ m ^. CF.monsterSpells) $ \(s, n) ->
        H.div_
          [ CSS.style_ [CSS.display "flex"]
          ]
          [ showSpell True c s,
            if n < 2 then M.vfrag [] else H.div_ [] [M.text $ M.toMisoString $ 'x' : show n]
          ],
      H.div_
        [ CSS.style_
            [ CSS.display "flex",
              CSS.justifyContent "space-between"
            ]
        ]
        [ if m ^. CF.isTapped then M.text "TODO-ICON" else H.div_ [] [],
          H.div_
            []
            [M.text $ M.toMisoString $ show $ m ^. CF.combatPower]
        ]
    ]
