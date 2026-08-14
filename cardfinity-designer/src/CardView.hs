{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module CardView (cardView, CardViewProps (CardViewProps)) where

import Context (Context, theme)
import Data.Foldable (Foldable (toList))
import Data.List (intersperse)
import Data.Maybe (isNothing)
import Miso qualified as M
import Miso.CSS qualified as CSS
import Miso.Html qualified as H
import Miso.Html.Property qualified as P
import Miso.Lens qualified as M
import Miso.Lens.TH (makeLenses)
import Optics.Operators ((^.))
import Scale (LegalityIssue (ScaleTooHigh), runScale)
import Shared qualified
import ShowCard (show'Spell)
import Theme.Types (themeClass)
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

data CardViewAction = ToggleConcise | TogglePrint

cardView :: M.Component Context CardViewProps CardViewModel CardViewAction
cardView = (M.component modelDefault update view) {M.useContext = True}
  where
    modelDefault = CardViewModel False False
    update ToggleConcise = conciseView M.%= not
    update TogglePrint = printView M.%= not

view :: Context -> CardViewProps -> CardViewModel -> M.View Context CardViewModel CardViewAction
view ctx props m =
  H.div_
    []
    [ H.span_
        []
        [ H.button_ [H.onClick ToggleConcise] [M.text $ if m M.^. conciseView then "Full" else "Concise"]
        ],
      -- TODO: print view
      if m M.^. printView
        then M.text "TODO: display entire deck for printing"
        else viewCard ctx props m,
      let cardScale = runScale (props M.^. deck) (props M.^. card)
       in H.p_
            [ CSS.style_
                [ CSS.display $ case cardScale of Left _ -> "block"; _ -> "none",
                  CSS.color CSS.red
                ]
            ]
            (case cardScale of Left issue -> [M.text $ M.toMisoString $ show issue]; Right _ -> []),
      H.pre_ [] [M.text $ M.toMisoString $ CF.unparse (m M.^. conciseView) (props M.^. card)]
    ]

viewCard :: Context -> CardViewProps -> CardViewModel -> M.View Context CardViewModel CardViewAction
viewCard ctx props m =
  H.div_
    [ CSS.style_
        [ CSS.aspectRatio "2.5 / 3.5",
          CSS.boxSizing "border-box",
          CSS.border "solid 2mm black",
          CSS.maxWidth "2.5in",
          CSS.maxHeight "3.5in",
          CSS.display "flex",
          CSS.flexDirection "column",
          CSS.padding "0.57% 1%",
          CSS.gap $ CSS.pct 0.5
        ],
      P.className $ themeClass $ ctx M.^. theme
    ]
    [ -- Top Row
      H.span_
        [P.classes_ ["title-bar"]]
        [ H.span_
            [ CSS.style_ [CSS.marginBottom $ CSS.pct 0.5],
              P.classes_ ["scale"]
            ]
            [showScale (props M.^. deck) (props M.^. card)],
          H.span_
            [ CSS.style_ [CSS.marginLeft $ CSS.em 1],
              P.classes_ ["card-name"]
            ]
            [showName (props M.^. card)]
        ],
      showImage (CF.isMonster $ props M.^. card) $ props M.^. card ^. CF.cardImageUrl,
      H.div_
        [ CSS.style_ [CSS.display "flex", CSS.justifyContent "space-evenly"],
          P.classes_ ["families-bar"]
        ]
        (map (H.em_ [] . (: []) . M.text . M.toMisoString) $ toList $ props M.^. card ^. CF.cardFamilies),
      CF.cardStatsElim (showSpell False $ m M.^. conciseView) (showMonster $ m M.^. conciseView) (props M.^. card ^. CF.cardStats)
    ]

showName :: CF.Card -> M.View ctx model action
showName c =
  let name = CF.cardName c
   in if name == "" then H.em_ [] [M.text "No Name"] else M.text $ M.toMisoString name

showScale :: [CF.Card] -> CF.Card -> M.View ctx model action
showScale deckList = M.text . either showLegalityIssue M.toMisoString . runScale deckList
  where
    showLegalityIssue (ScaleTooHigh _ s _) = M.toMisoString s
    showLegalityIssue _ = "?"

showImage :: Bool -> Maybe String -> M.View ctx model action
showImage isMonster mbUrl =
  H.img_
    [ P.src_ $ maybe fallback M.toMisoString mbUrl,
      CSS.style_ $ CSS.aspectRatio "2.5 / 2" : [CSS.width "100%" | isNothing mbUrl],
      P.classes_ ["card-image"]
    ]
  where
    fallback = if isMonster then "assets/icons/snail.svg" else "assets/icons/shell.svg"

showText :: Bool -> CF.CardText -> [M.View ctx model action]
showText False (CF.Text t) = [M.text $ M.toMisoString t]
showText True (CF.Text _) = [M.text " "]
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

showSpell :: Bool -> Bool -> CF.Spell -> M.View ctx model action
showSpell name c = H.span_ [P.classes_ ["spell-text"]] . showText c . show'Spell name

showMonster :: Bool -> CF.Monster -> M.View ctx model action
showMonster c m =
  H.div_
    [ CSS.style_
        [ CSS.display "flex",
          CSS.flexDirection "column",
          CSS.justifyContent "space-between",
          CSS.height "100%"
        ],
      P.classes_ ["monster-text"]
    ]
    [ H.span_
        [ P.classes_ ["monster-summoning-conditions"]
        ]
        $ showText c conditionsText,
      M.vfrag $ flip map (CF.collapse $ m ^. CF.monsterSpells) $ \(s, n) ->
        H.div_
          [ CSS.style_ [CSS.display "flex"],
            P.classes_ ["monster-spell"]
          ]
          [ showSpell True c s,
            if n < 2 then M.vfrag [] else H.div_ [] [M.text $ M.toMisoString $ 'x' : show n]
          ],
      H.div_
        [ CSS.style_
            [ CSS.display "flex",
              CSS.justifyContent "space-between",
              CSS.alignItems "center"
            ],
          P.classes_ ["monster-bar"]
        ]
        [ H.img_ [P.src_ "assets/icons/turtle.svg", CSS.style_ [CSS.visibility $ if m ^. CF.isTapped then "visible" else "hidden"]],
          H.div_
            [P.classes_ ["monster-power"]]
            [M.text $ M.toMisoString $ show $ m ^. CF.combatPower]
        ]
    ]
  where
    conditionsText = mconcat $ intersperse (CF.txt ", ") $ map CF.show' $ toList $ m ^. CF.summoningConditions
