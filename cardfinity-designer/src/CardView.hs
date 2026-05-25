{-# LANGUAGE OverloadedStrings #-}

module CardView (card) where

import Atoms qualified as CF
import Data.Foldable (Foldable (toList))
import Data.List (intercalate)
import Data.Maybe (fromMaybe, isNothing)
import Data.Set.Ordered qualified as OS
import Miso qualified as M
import Miso.CSS qualified as CSS
import Miso.Html qualified as H
import Miso.Html.Property qualified as P
import Miso.Lens (Lens, lens)
import Miso.Lens.TH (makeLenses)
import Optics.Operators ((^.))
import Scale (runScale)
import Shared qualified
import ShowCard (show'Spell)
import Types qualified as CF

card :: Bool -> [CF.Card] -> CF.Card -> M.View model action
card printMode deck c =
  H.div_
    [ CSS.style_
        [ CSS.aspectRatio "2.5 / 3.5",
          CSS.border "solid 1px black",
          CSS.maxWidth $ if printMode then "2.5in" else CSS.vw 40,
          CSS.maxHeight $ if printMode then "3.5in" else CSS.vh 56,
          CSS.display "flex",
          CSS.flexDirection "column"
        ]
    ]
    [ -- Top Row
      H.div_
        [ CSS.style_
            [ CSS.display "flex"
            ]
        ]
        [ H.p_ [CSS.style_ [CSS.margin $ CSS.em 0.5]] [showScale deck c],
          H.p_
            [ CSS.style_ [CSS.margin $ CSS.em 0.5]
            ]
            [showName c]
        ],
      -- Image
      showImage $ c ^. CF.cardImageUrl,
      H.div_ [CSS.style_ [CSS.display "flex"]] (map (H.em_ [] . (: []) . M.text . M.toMisoString) $ toList $ c ^. CF.cardFamilies),
      CF.cardStatsElim (showSpell False) (showMonster False) (c ^. CF.cardStats)
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
showText True (CF.Text t) = [M.text $ M.toMisoString t]
showText False (CF.Text t) = [M.text $ M.toMisoString " "]
showText _ (CF.Number n) = [M.text $ M.toMisoString $ show n]
showText _ (CF.Trigger t) = [Shared.triggerIcon t]
showText _ (CF.CardName f) = [M.text $ M.toMisoString $ show f]
showText False (CF.CardFamily f) = [M.text $ M.toMisoString $ show f]
showText True (CF.CardFamily f) = [M.text $ M.toMisoString $ 'f' : show f]
showText _ (CF.Keyword w) = [H.em_ [] [M.text $ M.toMisoString w]]
showText c (CF.Branch l r) = showText c l ++ showText c r
showText c (CF.List xs) = [H.ul_ [] $ flip map (toList xs) $ \x -> H.li_ [] (showText c x)]

showSpell :: Bool -> CF.Spell -> M.View model action
showSpell c = H.span_ [] . showText c . show'Spell False

showMonster :: Bool -> CF.Monster -> M.View model action
showMonster c = M.vfrag . showText c . CF.show'
